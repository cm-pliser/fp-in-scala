import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success}
import scala.util.control.Breaks._

object CountDownLatch {

    import java.util.concurrent.atomic.AtomicInteger
    import scala.util.Random

    // Atomicカウンタ
    val index = new AtomicInteger(0)
    // 1000までの範囲でランダム値
    def getWaitMiliSeconds():Int = new Random().nextInt(1000)

    val futureArray = Array(createFuture, createFuture, createFuture, createFuture, createFuture, createFuture, createFuture, createFuture)
    val promiseArray = Array(Promise[Int], Promise[Int], Promise[Int])
    
    def main(args: Array[String]): Unit = {
        // この↓からほぼコピペ
        // futureArray.foreach { f1 => // futureを1個ずつ取り出す
        //     f1.foreach {    // Future.successfulと同等だとか？
        //         case waitMil: Int => 
        //             val idx = index.getAndIncrement
        //             if (idx < promiseArray.length) {
        //                 println(s"current idx: $idx")
        //                 promiseArray(idx).success(waitMil)
        //             } else {
        //                 println("結果の先着受付は終了しました")
        //             }
        //         }

        //     promiseArray.foreach { p =>
        //         p.future.foreach {      // Future.successfulと同等？
        //             case waitMilliSec => println(waitMilliSec)  // 完了しているPromiseだけ出力される
        //         } 
        //     }
        //     Thread.sleep(5000)
        //     println("5 sec")
        // }

        // なんとか終わらせてみたかった
        var idx2 = 0
        futureArray.iterator.takeWhile(_ => idx2 < promiseArray.length).foreach { f1 => // futureを1個ずつ取り出す
            f1.foreach {    // Future.successfulと同等だとか？
                case waitMil: Int => 
                    val idx = index.getAndIncrement
                    if (idx < promiseArray.length) {
                        println(s"current idx: $idx")
                        promiseArray(idx).success(waitMil)
                    } else {
                        println("集計終わったのでおしまい！")
                    }
                    idx2 = idx
                }

            promiseArray.foreach { p =>
                p.future.foreach {      // Future.successfulと同等？
                    case waitMilliSec => println(waitMilliSec)  // 完了しているPromiseだけ出力される
                } 
            }
        }
        Thread.sleep(5000)
        println("5 sec")
    }

    def createFuture(): Future[Int] = 
    Future {
        val waitMiliSeconds = getWaitMiliSeconds()
        Thread.sleep(waitMiliSeconds)
        waitMiliSeconds
    }
}