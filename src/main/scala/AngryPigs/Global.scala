package AngryPigs

import org.lwjgl.util.glu.{Sphere,Cylinder,Disk,PartialDisk}
import scala.collection.mutable.{ListBuffer,HashMap}
// stuff that is used in all the (wrong) places :P
// ... it's made of fail and state
object Global {
  object Settings {
    var graphics = 1 // polygon multiplier
    var maxdepth = 5 // tree depth
    var pigAir = false // lol, pig fly ///@ wrong - model property, and collision related :P
    var worldSize = 400
    var gravity = Vec3(0f,-0.5f,0f)
  }
  def settings: SettingMap[String] = { // old settings - idfunc in models depends on this :/ ///could maybe do via reflection and implicits
    val m = new SettingMap[String]
    m += "graphics" -> Settings.graphics
    m += "maxdepth" -> Settings.maxdepth
    m
  }
  var tasks = List[() => Unit]()
  
  object gluQuadrics {
    val sphere = new Sphere
    val cylinder = new Cylinder
    val disk = new Disk
    val partialdisk = new PartialDisk
  }

  val genTree = new ClojureWrap("AngryPigs", "gen-tree")
}

object Utils {
  implicit class D(val d: Double) { def prob: Boolean = util.Random.nextDouble < d } //0.5.prob #syntaxabuse
  implicit class F(val f: Float) { def prob: Boolean = util.Random.nextFloat < f }

  def withAlternative[T](func: => T, alternative: => T ): T = try { func } catch { case _: Throwable => alternative}
  def withExit[T](func: => T, exit: => Any = { }): T = try { func } catch { case _: Throwable => exit; sys.exit(-1) }

  def currentTime: Long = System.nanoTime()
  // measures the running time of the provided func
  def time(func: => Unit): Long = {
    val startTime = currentTime
    func
    (currentTime-startTime)
  }
}

// some small classes

class SettingMap[A] extends HashMap[A,Any] {
  private val defaultMap = new HashMap[String, Any]
  def setDefault[B](v: B)(implicit m: Manifest[B]): Unit = defaultMap += m.toString -> v
  def getDefault[B](implicit m: Manifest[B]): B = defaultMap.getOrElse(m.toString, null).asInstanceOf[B]
  
  def get[B: Manifest](key: A): B = getOrElse(key, getDefault[B]).asInstanceOf[B]
  // add trigger hooks for when some value updates :P
}
trait Properties {
  val properties = new SettingMap[String]
}


class TimeLock {
  private var locked = false
  def isLocked: Boolean = {
    if(locked && milliTime-lockTime > lockDuration) locked = false
    
    locked
  }
  
  private def milliTime: Long = System.nanoTime()/1000000L
  
  private var lockTime = milliTime
  private var lockDuration = 0L
  def lockIt(ms: Int) {
    lockTime = milliTime
    lockDuration = ms
    locked = true
  }
}
