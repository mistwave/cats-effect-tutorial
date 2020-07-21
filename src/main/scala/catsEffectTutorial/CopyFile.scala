/*
 * Copyright (c) 2018 Luis Rodero-Merino
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
package catsEffectTutorial

import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._
import java.io._
import scala.io.StdIn

object CopyFile extends IOApp {

  def transmit[F[_] : Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].delay(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1) Sync[F].delay(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else Sync[F].pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted


  def transfer[F[_] : Sync](origin: InputStream, destination: OutputStream): F[Long] =
    for {
      buffer <- Sync[F].delay {
        new Array[Byte](1024 * 10)
      } // Allocated only when the IO is evaluated
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def inputStream[F[_] : Sync](f: File, guard: Semaphore[F]): Resource[F, FileInputStream] =
    Resource.make {
      Sync[F].delay(new FileInputStream(f))
    } { inStream =>
      guard.withPermit {
        Sync[F].delay(inStream.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

  def outputStream[F[_] : Sync](f: File, guard: Semaphore[F]): Resource[F, FileOutputStream] =
    Resource.make {
      Sync[F].delay(new FileOutputStream(f))
    } { outStream =>
      guard.withPermit {
        Sync[F].delay(outStream.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

  def inputOutputStreams[F[_] : Sync](in: File, out: File, guard: Semaphore[F]): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in, guard)
      outStream <- outputStream(out, guard)
    } yield (inStream, outStream)

  def copy[F[_] : Concurrent](origin: File, destination: File): F[Long] =
    for {
      guard <- Semaphore[F](1)
      count <- inputOutputStreams(origin, destination, guard).use { case (in, out) =>
        guard.withPermit(transfer(in, out))
      }
    } yield count

  // The 'main' function of IOApp //
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- validateArgsE(args) match {
        case Left(e) => IO.raiseError(e)
        case _ => IO.unit
      }
      orig = new File(args.head)
      dest = new File(args.tail.head)
      _ <- if (orig.canRead) IO.unit else IO.raiseError(new IllegalArgumentException("origin file cannot be read from"))
      _ <- if (dest.canWrite) IO.unit else IO.raiseError(new IllegalArgumentException("destination file cannot be written to"))
      iow <- IO(StdIn.readLine("Destination file existed, [Y]es to OVERWRITE: "))
      _ <- iow match {
        case "Y" =>
          copy[IO](orig, dest).flatMap(count =>
            IO(println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")))
        case _ => IO.unit
      }
    } yield ExitCode.Success
  }

  def validateArgsE(args: List[String]): Either[Exception, Unit] = args match {
    case _ if args.length < 2 => Left(new IllegalArgumentException("Need origin and destination files"))
    case _ if args.head == args(1) => Left(new IllegalArgumentException("Origin and destination cannot be the same"))
    case _ => Right()
  }

}
