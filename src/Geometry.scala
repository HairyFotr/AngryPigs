import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.util.glu._

class Vec3(var x:Float, var y:Float, var z:Float) {
    def this() = this(0f,0f,0f);
    
    override def toString = "%.2f, %.2f, %.2f".format(x,y,z);
}
class Quad(var p1:Vec3, var p2:Vec3, var p3:Vec3, var p4:Vec3) {
    def getPoints = List(p1,p2,p3,p4);
}

// @most models will be static - compile them into displaylists / VBOs
// lazily on first render vs loading time?
// catapult as model-chain - has static parts which move

abstract class BasicModel {
    var (pos,rot,scale) = (new Vec3(0f,0f,0f), new Vec3(0f,0f,0f), new Vec3(0f,0f,0f));

    def setPosition(x:Float,y:Float,z:Float) = { pos = new Vec3(x,y,z); }
    def setRotation(x:Float,y:Float,z:Float) = { rot = new Vec3(x,y,z); }
    def setScale(x:Float,y:Float,z:Float) = { scale = new Vec3(x,y,z); }
    
    // How do I render this model?
    def render;

    override def toString:String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")";
}
abstract class PointsModel extends BasicModel {
    var points:Array[Vec3];
}

// doesn't care about 
//class CompiledModel extends BasicModel {
    
//}

class Camera extends BasicModel {
    // default projection 
    var perspective = false
    var (near,far) = (1f,30f) // near, far clipping plane
    var (fov,aspectRatio) = (45f,4/3f) // perspective stuff
    var (minX,minY,maxX,maxY) = (-1f,-1f, 1f, 1f) // ortho stuff

    // set a perspective projection
    def setPerspective(fv:Float, ar:Float, n:Float, f:Float) {
        perspective=true
        fov=fv
        aspectRatio=ar
        near=n
        far=f
    }
  
    // set an ortographic projection
    def setOrtho(mx:Float, my:Float, Mx:Float, My:Float, n:Float, f:Float) {
        perspective=false
        minX=mx
        minY=my
        maxX=Mx
        maxY=My
        near=n
        far=f
    }
  
    override def render {
        // setup projection matrix stack
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity();
        if(perspective) {
            // perspective projection
            GLU.gluPerspective(fov,aspectRatio, near,far);
        } else {
            // orthographic projection 
            GL11.glOrtho(minX,maxX, minY,maxY, near,far);
        }

        // model view stack 
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity();
        
        // setup view space; 
        GL11.glTranslatef(pos.x, pos.y, pos.z);

        if(rot.z!=0) GL11.glRotatef(rot.z, 0, 0, 1);
        if(rot.y!=0) GL11.glRotatef(rot.y, 0, 1, 0);
        if(rot.x!=0) GL11.glRotatef(rot.x, 1, 0, 0);
    }
}

// @maybe normalize width&height to 1 and then scale?
class QuadPatch extends PointsModel {
    var width = 1
    var points:Array[Vec3]=null
    var texPoints:Array[Vec3]=null
    var displayList = -1
    
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

    override def render {
        GL11.glPushMatrix;
        
        GL11.glTranslatef(pos.x, pos.y, pos.z);
        GL11.glRotatef(1f, rot.x, rot.y, rot.z);

        if (displayList == -1) {
            displayList = GL11.glGenLists(1);
            GL11.glNewList(displayList,GL11.GL_COMPILE_AND_EXECUTE);
            GL11.glBegin(GL11.GL_QUADS);
            // Draw in clockwise - (00,01,11,10); must skip last point of line.
            for(i <- 0 until points.length-width-1; if((i+1)%width > 0)) {
                List(points(i), points(i+width), points(i+width+1), points(i+1)).foreach {
                    (p:Vec3) => {
                        GL11.glColor3f(p.y/5, p.y, p.y/5);
                        //GL11.glTexCoord2f(clockwise(j)._1,clockwise(j)._2);
                        GL11.glVertex3f(p.x, p.y, p.z);
                    }
                }
            }
            GL11.glEnd;
            GL11.glEndList;
        } else {
            GL11.glCallList(displayList);
        }
        
        GL11.glPopMatrix;
    }

    // @make a method to merge quadpatches (for infinite terrain)   
}
