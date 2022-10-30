
package dev.zio.quickstart.testTask

import zio.*
import zio.json.*
import zhttp.http.*
import zhttp.http.HttpError.BadRequest
import scala.io.Source
import scala.*

val e = new Exception("Failed to parse the input")
val blackList = Source.fromResource("black_list.txt").getLines().toList.map(_.toInt) //получаем список из требуемого файла в ресурсах

object CheckApp:

  def apply(): Http[Any, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case req@(Method.POST -> !! / "transaction-check") =>
        for
          body <- req.bodyAsString // тело реквеста
          transaction <- ZIO.fromEither(body.fromJson[Transaction]).mapError{ case t => e}  //парсим тело реквеста и если не можем распарсить-выкидываем ошибку
          successSrc <- ZIO.succeed(!blackList.contains(transaction.src)) //boolean value для проверки src
          successDst <- ZIO.succeed(!blackList.contains(transaction.dst)) //boolean value для проверки dst
          result <- ZIO.from( // "разветвил" проверку для локализации ошибки в реквесте
            if (!successSrc) { //if successSrc=false(т.е blacklist содержит значение в src) - реквест маркируется как BadRequest
              Response.text("Cancel (src is blacklisted)").setStatus(Status.BadRequest)
            } else if (!successDst) { //if successDst=false(т.е blacklist содержит значение в dst) - реквест маркируется как BadRequest
              Response.text("Cancel (dst is blacklisted)").setStatus(Status.BadRequest)
            } else { // в противном случае понимаем, что ни src ни dst не находятся в blacklist
              Response.text("Success").setStatus(Status.Ok)
            }
          )
        yield result
    }


