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

import com.github.jarlakxen.drunk.extensions._
import io.circe._

class GraphQLCursor[F[_], Res, Vars](
  client: GraphQLClient[F],
  val result: F[GraphQLClient.GraphQLResponse[Res]],
  val extensions: F[GraphQLExtensions],
  val lastOperation: GraphQLOperation[Res, Vars])(implicit responseDecoder: Decoder[Res]) {

  def refetch: GraphQLCursor[F, Res, Vars] =
    refetch(None)

  def fetchMore(variables: Vars): GraphQLCursor[F, Res, Vars] =
    refetch(Some(variables))

  def fetchMore(newVars: Vars => Vars): GraphQLCursor[F, Res, Vars] =
    refetch(lastOperation.variables.map(newVars(_)))

  private def refetch(variables: Option[Vars]): GraphQLCursor[F, Res, Vars] = {
    implicit val variablesEncoder = lastOperation.variablesEncoder
    client.query(lastOperation.doc, variables, lastOperation.name)
  }
}