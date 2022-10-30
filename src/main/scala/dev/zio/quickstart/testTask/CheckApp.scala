
package dev.zio.quickstart.testTask

import zio.*
import zio.json.*
import zhttp.http.*
import zhttp.http.HttpError.BadRequest
import scala.io.Source
import scala.*

val e = new Exception("Failed to parse the input")
val blackList = Source.fromResource("black_list.txt").getLines().toList.map(_.toInt) //get list from resource file

object CheckApp:

  def apply(): Http[Any, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case req@(Method.POST -> !! / "transaction-check") =>
        for
          body <- req.bodyAsString
          transaction <- ZIO.fromEither(body.fromJson[Transaction]).mapError{ case t => e}
          succsessSrc <- ZIO.succeed(!blackList.contains(transaction.src)) //boolean value for check src
          succsessDst <- ZIO.succeed(!blackList.contains(transaction.dst)) //boolean value for check dst
          result <- ZIO.from(
            if (!succsessSrc) {
              Response.text("Cancel (src is blacklisted)").setStatus(Status.BadRequest)
            } else if (!succsessDst) {
              Response.text("Cancel (dst is blacklisted)").setStatus(Status.BadRequest)
            } else {
              Response.text("Success").setStatus(Status.Ok)
            }
          )
        yield result
    }


