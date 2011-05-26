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
    
    def time(name:String = "")(f: =>Unit):Int = {
        val startTime = System.nanoTime();
        
        f;
        
        ((System.nanoTime()-startTime)).toInt
    }

    // OH THE HUGE MANATEEE!    
    // just traverses some weird tree strucure
    // don't read this, pls!
    def traverseTree(data:Object, branchFunc:(Array[Float], Array[Float], Int)=>Array[Float]) {
        import java.util.{List=>JavaList}
        
        def isJavaList(a:Object):Boolean = a.isInstanceOf[JavaList[Object]]
        def asArray(a:Object):Array[Object] = a.asInstanceOf[JavaList[Object]].toArray;
        def asFloatArray(a:Array[Object]):Array[Float] = a.toList.map(
                (a)=>{
                    if(a.isInstanceOf[java.lang.Double])
                        a.asInstanceOf[java.lang.Double].floatValue();
                    else
                        a.asInstanceOf[Float]
                }
            ).toArray

        var depth=0;
        def traverse(vec:Array[Float], data:Array[Object]):Array[Float] = {
            if(data.length==4 && !isJavaList(data(0))) {
                val vector = asFloatArray(data);
                
                return branchFunc(vector, vec, depth);
            } else {
                depth += 1;
                if(data.length==1 || !isJavaList(asArray(data(1)).apply(0)))
                    for(i <- 0 until data.length) traverse(vec, asArray(data(i)))
                else {
                    var root = traverse(vec, asArray(data(0)));
                    for(i <- 1 until data.length) traverse(root, asArray(data(i)))
                }
                depth -= 1;

                // gotta return something...
                return vec;
            }
        }
        
        traverse(Array[Float](0,0,0,1), asArray(data))
    }
}

// some small classes

class SettingMap[A] extends scala.collection.mutable.HashMap[A,Any] {
    private var defaultMap = new scala.collection.mutable.HashMap[String, Any]
    def setDefault[B](v:B)(implicit m:Manifest[B]) = defaultMap += m.toString -> v
    def getDefault[B](implicit m:Manifest[B]):B = defaultMap.getOrElse(m.toString, null).asInstanceOf[B];
    
    def get[B:Manifest](key:A):B = getOrElse(key, getDefault[B]).asInstanceOf[B];
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
