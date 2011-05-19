package AngryPigs

import org.lwjgl.util.glu._
import scala.util._

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
}

