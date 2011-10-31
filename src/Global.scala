package AngryPigs

import scala.util.Random
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
        var gravity = new Vec3(0f,-0.5f,0f)
    }
    def settings:SettingMap[String] = { // old settings - idfunc in models depends on this :/ ///could maybe do via reflection and implicits
        val m = new SettingMap[String]
        m += "graphics" -> Settings.graphics
        m += "maxdepth" -> Settings.maxdepth
        m;
    }
    lazy val tasks = new ListBuffer[() => Unit]
    lazy val urgentTasks = new ListBuffer[() => Unit]
    lazy val rand = new Random
    implicit def double2float(d:Double):Float = d.toFloat
    
    object gluQuadrics {
        val sphere = new Sphere
        val cylinder = new Cylinder
        val disk = new Disk
        val partialdisk = new PartialDisk
    }

    lazy val genTree = new ClojureWrap("AngryPigs", "gen-tree")

    def currentTime = System.nanoTime()
    // measures the running time of the provided func    
    def time(func: => Unit):Long = {
        val startTime = currentTime
        func
        (currentTime-startTime)
    }
}

// some small classes

class SettingMap[A] extends scala.collection.mutable.HashMap[A,Any] {
    private var defaultMap = new scala.collection.mutable.HashMap[String, Any]
    def setDefault[B](v:B)(implicit m:Manifest[B]) = defaultMap += m.toString -> v
    def getDefault[B](implicit m:Manifest[B]):B = defaultMap.getOrElse(m.toString, null).asInstanceOf[B]
    
    def get[B:Manifest](key:A):B = getOrElse(key, getDefault[B]).asInstanceOf[B]
    // add trigger hooks for when some value updates :P
}
trait Properties {
    var properties = new SettingMap[String]
}


class TimeLock {
    private var locked = false
    def isLocked:Boolean = {
        if(locked && milliTime-lockTime>lockDuration) locked = false
        
        locked
    }
    
    private def milliTime = System.nanoTime()/1000000L
    
    private var lockTime = milliTime
    private var lockDuration = 0L
    def lockIt(ms:Int) = {
        lockTime = milliTime
        lockDuration = ms
        locked = true
    }
}
