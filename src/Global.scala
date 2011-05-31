package AngryPigs

import scala.util.Random
import org.lwjgl.util.glu.{Sphere,Cylinder,Disk,PartialDisk}

// stuff that is used in all the (wrong) places :P
// ... it's made of fail and state
object Global {
    var settings = new SettingMap[String];
    settings += "graphics" -> 2f; // polygon multiplier for generatives.
    settings += "fattrees" -> true;

    val rand = new Random;
    
    object gluQuadrics {
        val sphere = new Sphere;
        val cylinder = new Cylinder;
        val disk = new Disk;
        val partialdisk = new PartialDisk;
    }

    val genTree = new ClojureWrap("AngryPigs", "gen-tree");

    // measures the running time of the provided func    
    def time(name:String = "")(f: =>Unit):Long = {
        val startTime = System.nanoTime();
        
        f;
        
        (System.nanoTime()-startTime)
    }
}

// some small classes

class SettingMap[A] extends scala.collection.mutable.HashMap[A,Any] {
    private var defaultMap = new scala.collection.mutable.HashMap[String, Any]
    def setDefault[B](v:B)(implicit m:Manifest[B]) = defaultMap += m.toString -> v
    def getDefault[B](implicit m:Manifest[B]):B = defaultMap.getOrElse(m.toString, null).asInstanceOf[B];
    
    def get[B:Manifest](key:A):B = getOrElse(key, getDefault[B]).asInstanceOf[B];
    // add trigger hooks for when some value updates :P
}

class TimeLock {
    private var locked = false;
    def isLocked:Boolean = {
        if(locked && milliTime-lockTime > lockDuration) locked = false;
        
        locked;
    }
    
    private def milliTime = System.nanoTime()/1000000L
    
    private var lockTime = milliTime;
    private var lockDuration = 0L;
    def lockIt(ms:Int) = {
        lockTime = milliTime;
        lockDuration = ms;
        locked = true
    }
}
