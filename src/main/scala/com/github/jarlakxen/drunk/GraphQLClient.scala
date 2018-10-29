/*
 * Copyright 2018 Facundo Viale
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.jarlakxen.drunk

import scala.util._
import com.softwaremill.sttp._
import extensions.{GraphQLExtensions, NoExtensions}
import io.circe._
import io.circe.parser._
import sangria.ast.Document
import sangria.introspection._
import sangria.marshalling.circe._
import sangria.parser.QueryParser

class GraphQLClient[F[_]] private[GraphQLClient] (uri: Uri, options: ClientOptions, headers: Seq[(String, String)])(
  implicit backend: SttpBackend[F, Nothing]
) {
  import GraphQLClient._

  private val rm: MonadError[F] = backend.responseMonad

  private[drunk] def execute[Res, Vars](doc: Document, variables: Option[Vars], name: Option[String])(
    implicit variablesEncoder: Encoder[Vars]
  ): F[(Int, Json)] =
    execute(GraphQLOperation(doc, variables, name))

  private def buildRequest(body: String): RequestT[Id, String, Nothing] =
    sttp
      .post(uri)
      .body(body)
      .response(asString)
      .headers(headers: _*)

  private[drunk] def execute[Res, Vars](op: GraphQLOperation[Res, Vars]): F[(Int, Json)] = {

    val fields =
      List("query" -> op.docToJson) ++
        op.encodeVariables.map("variables" -> _) ++
        op.name.map("operationName" -> Json.fromString(_))

    val body: String = Json.obj(fields: _*).noSpaces
    val request = buildRequest(body)

    rm.flatMap(request.send()) {
      case Response(rawErrorBody, code, _, _, _) =>
        val body = rawErrorBody.left
          .map(new String(_))
          .merge

        rm.fromTry(parse(body).toTry.map(code -> _))
    }
  }

  def query[Res](doc: String)(implicit dec: Decoder[Res]): Try[GraphQLCursor[F, Res, Nothing]] =
    query(doc, None, None)(dec, null)

  def query[Res](
    doc: String,
    operationName: String
  )(implicit dec: Decoder[Res]): Try[GraphQLCursor[F, Res, Nothing]] =
    query(doc, None, Some(operationName))(dec, null)

  def query[Res, Vars](doc: String, variables: Vars)(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars]
  ): Try[GraphQLCursor[F, Res, Vars]] =
    query(doc, Some(variables), None)

  def query[Res, Vars](doc: String, variables: Option[Vars], operationName: Option[String])(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars]
  ): Try[GraphQLCursor[F, Res, Vars]] =
    QueryParser.parse(doc).map(query(_, variables, operationName))

  def query[Res](doc: Document)(implicit dec: Decoder[Res]): GraphQLCursor[F, Res, Nothing] =
    query(doc, None, None)(dec, null)

  def query[Res](
    doc: Document,
    operationName: String
  )(implicit dec: Decoder[Res]): GraphQLCursor[F, Res, Nothing] =
    query(doc, None, Some(operationName))(dec, null)

  def query[Res, Vars](doc: Document, variables: Vars)(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars]
  ): GraphQLCursor[F, Res, Vars] =
    query(doc, Some(variables), None)

  def query[Res, Vars](doc: Document, variables: Option[Vars], operationName: Option[String])(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars]
  ): GraphQLCursor[F, Res, Vars] = {
    var fullDoc = doc
    if (options.addTypename) {
      fullDoc = ast.addTypename(doc)
    }

    val operation: GraphQLOperation[Res, Vars] = GraphQLOperation(doc, variables, operationName)
    val result = execute(operation)
    val data: F[GraphQLClient.GraphQLResponse[Res]] = rm.flatMap(result) {
      case (status, body) => rm.fromTry(extractErrorOrData(body, status))
    }
    val extensions = rm.map(result) { case (_, body) => extractExtensions(body) }
    new GraphQLCursor(this, data, extensions, operation)
  }

  def mutate[Res, Vars](doc: Document, variables: Vars)(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars]
  ): F[GraphQLResponse[Res]] =
    mutate(doc, Some(variables), None)

  def mutate[Res, Vars](doc: Document, variables: Vars, operationName: Option[String])(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars]
  ): F[GraphQLResponse[Res]] = {

    val result = execute(doc, Some(variables), operationName)
    rm.flatMap(result) { case (status, body) => rm.fromTry(extractErrorOrData(body, status)) }
  }

  def schema: F[IntrospectionSchema] =
    rm.flatMap(execute[Json, Nothing](introspectionQuery, None, None)(null)) {
      case (_, json) => rm.fromTry(IntrospectionParser.parse(json))
    }

}

object GraphQLClient {

  // Work arround for Scala 2.11
  implicit class Either212[+A, +B](either: Either[A, B]) {

    def toTry(implicit ev: A <:< Throwable): Try[B] = either match {
      case Right(b) => Success(b)
      case Left(a)  => Failure(a)
    }

    def toOption: Option[B] = either match {
      case Right(b) => Some(b)
      case _        => None
    }
  }

  type GraphQLResponse[Res] = Either[GraphQLResponseError, GraphQLResponseData[Res]]

  def apply[F[_]](
    uri: String,
    clientOptions: ClientOptions = ClientOptions.Default,
    headers: Seq[(String, String)] = Seq.empty
  )(implicit backend: SttpBackend[F, Nothing]): GraphQLClient[F] =
    new GraphQLClient(uri"$uri", clientOptions, headers)(backend)

  private[GraphQLClient] def extractErrors(body: Json, statusCode: Int): Option[GraphQLResponseError] = {
    val cursor: HCursor = body.hcursor

    for {
      errorsNode <- cursor.downField("errors").focus
      errors <- errorsNode.asArray
    } yield {
      val msgs = errors.flatMap(_.hcursor.downField("message").as[String].toOption).toList
      GraphQLResponseError(msgs, statusCode)
    }
  }

  private[GraphQLClient] def extractData[Res](
    jsonBody: Json
  )(implicit dec: Decoder[Res]): Try[GraphQLResponseData[Res]] =
    jsonBody.hcursor.downField("data").as[Res].toTry.map(GraphQLResponseData(_))

  private[GraphQLClient] def extractErrorOrData[Res](jsonBody: Json, statusCode: Int)(
    implicit dec: Decoder[Res]
  ): Try[GraphQLResponse[Res]] = {
    val errors: Option[Try[GraphQLResponse[Res]]] =
      extractErrors(jsonBody, statusCode).map(errors => Success(Left(errors)))
    val data: Try[GraphQLResponse[Res]] =
      extractData(jsonBody).map(Right(_))

    errors.getOrElse(data)
  }

  private[GraphQLClient] def extractExtensions(jsonBody: Json): GraphQLExtensions =
    jsonBody.hcursor
      .downField("extensions")
      .as[GraphQLExtensions]
      .toOption
      .getOrElse(NoExtensions)

}
