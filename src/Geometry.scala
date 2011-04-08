import org.lwjgl._
import org.lwjgl.opengl._

class Vec3(var x:Float, var y:Float, var z:Float) {
    def this() = this(0f,0f,0f);
};
class Quad(var p1:Vec3, var p2:Vec3, var p3:Vec3, var p4:Vec3) {
    def getPoints = List(p1,p2,p3,p4);
}

// @most models will be static - compile them into displaylists
// lazily on first render vs loading time?
// catapult as model-chain - has static parts which move

abstract class Model {
    var points:Array[Vec3];
    var rot = new Vec3(0f,0f,0f);
    var pos = new Vec3(0f,0f,0f);
    var scale = new Vec3(0f,0f,0f);

    def setRotation(x:Float,y:Float,z:Float) = { rot = new Vec3(x,y,z); }
    def setPosition(x:Float,y:Float,z:Float) = { pos = new Vec3(x,y,z); }
    def setScale(x:Float,y:Float,z:Float) = { scale = new Vec3(x,y,z); }
    
    // How do I render this model?
    def render;
}

class QuadPatch extends Model {
    var width = 1;
    var points:Array[Vec3]=null;
    var texPoints:Array[Vec3]=null;
    
    // constructors
    // @take in random points and make it into a valid quadpatch
    //  also use quads
    def this(p:Array[Vec3], w:Int) { 
        this();
        width = w;
        points = p;
    }
    def this(p:Array[Vec3], tex:Array[Vec3], w:Int) {
        this(p, w);
        texPoints = tex;
    }

    def render {
        GL11.glBegin(GL11.GL_QUADS);
        // Draw in clockwise - (00,01,11,10); must skip last point of line.
        for(i <- 0 until points.length-width-1; if((i+1)%width > 0)) {
            List(points(i), points(i+width), points(i+width+1), points(i+1)).foreach {
                (p:Vec3) => {
                    GL11.glColor3f(p.y, p.y, p.y);
                    //GL11.glTexCoord2f(clockwise(j)._1,clockwise(j)._2);
                    GL11.glVertex3f(p.x, p.y, p.z-3);
                }
            }
        }
        GL11.glEnd;
    }
}
