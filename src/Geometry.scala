import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.util.glu._

class Vec3(var x:Float, var y:Float, var z:Float) {
    def this() = this(0f,0f,0f);
    
    def getPoints = List(x,y,z);
    def setPoints(p:List[Float]) = { x=p(0); y=p(1); z=p(2) }
  
    def map(f:Float=>Float) = {setPoints(getPoints.map(f)); this};
  
    // @dot product etc. when needed
    def +(v:Vec3):Vec3 = { var res=this.clone; res.x += v.x; res.y += v.y; res.z += v.z; res }
    def +=(v:Vec3) = { this.apply(this+v) }
    def *(f:Float):Vec3 = { var res=this.clone; res.map(_ * f); res };
    def applyVector(v:Vec3,multi:Float = 1) = { var res = this+(v*multi); this.apply(res) }
    def ==(v:Vec3):Boolean = (x==v.x && y==v.y && z==v.z)
    def !=(v:Vec3):Boolean = !(this==v)
    def apply(v:Vec3) = { this.x=v.x; this.y=v.y; this.z=v.z }
    
    override def clone = new Vec3(x,y,z);

    // clamp values to some value(e.g. world size)
    private def clamp(p:Float,clamp:Float):Float = if(math.abs(p) > clamp) clamp*(p/math.abs(p)) else p;
    def clamp(c:Float):Vec3 = this.map(clamp(_,c));
    def clamp3(cx:Float,cy:Float,cz:Float) = {
        x = clamp(x, cx);
        y = clamp(y, cy);
        z = clamp(z, cz);
    }

    override def toString = "%.2f, %.2f, %.2f".format(x,y,z);
}
// @ a lot of these classes could be traits i think... google mixin scala
// also, is this nice in any way? :) (SomeModel with Vector seems nice)
// also traits vs abstract classes vs java interfaces
// also i'm on a train with no internets
// also there seems to be no way to add traits inline as i had hoped... bummer.
trait Vector {
    var vector = new Vec3;

    def setVector(x:Float,y:Float,z:Float) = { vector = new Vec3(x,y,z); }
}
trait Points{
    var points:Array[Vec3]=null;
}

class Quad(var p1:Vec3, var p2:Vec3, var p3:Vec3, var p4:Vec3) {
    def getPoints = List(p1,p2,p3,p4);
  
    def foreach(f:Vec3=>Unit) = getPoints.foreach(f);
    def map(f:Vec3=>Unit) = foreach(f);
}

// @most models will be static - compile them into displaylists / VBOs
// lazily on first render vs loading time?
// catapult as model-chain - has static parts which move

abstract class BasicModel {
    var (pos,rot,scale) = (new Vec3, new Vec3, new Vec3(1f,1f,1f));

    def setPosition(x:Float,y:Float,z:Float) = { pos = new Vec3(x,y,z); }
    def setPosition(v:Vec3) = { pos = new Vec3(v.x,v.y,v.z); }
    def setRotation(x:Float,y:Float,z:Float) = { rot = new Vec3(x,y,z); }
    def setRotation(v:Vec3) = { rot = new Vec3(v.x,v.y,v.z); }
    def setScale(x:Float,y:Float,z:Float) = { scale = new Vec3(x,y,z); }
    def setScale(v:Vec3) = { scale = new Vec3(v.x,v.y,v.z); }
    
    // How do I render this model?
    def render;

    override def toString:String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")";
}

// doesn't care about points and stuff
class DisplayModel(var displayList:Int) extends BasicModel with Vector {
    def this(renderf:Unit=>Unit) {
        this(-1);
        renderfunc = renderf;
        displayList = GL11.glGenLists(1);
        GL11.glNewList(displayList,GL11.GL_COMPILE);
        renderfunc();
        GL11.glEndList;
    }
    var renderfunc:Unit=>Unit = Unit=>{};
    
    
    override def render {
        GL11.glPushMatrix;
        
        GL11.glTranslatef(pos.x, pos.y, pos.z);
        GL11.glRotatef(1f, rot.x, rot.y, rot.z);
        GL11.glScalef(scale.x, scale.y, scale.z);
        
        if(displayList == -1)
            renderfunc();
        else
            GL11.glCallList(displayList);
        
        GL11.glPopMatrix;
    }
    
}

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
        
        //wow, this scaling is weird :)
        GL11.glScalef(scale.x, scale.y, scale.z);
    }
}

// @maybe normalize width&height to 1 and then scale?
class QuadPatch extends BasicModel with Points {
    var width = 1
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
        GL11.glScalef(scale.x, scale.y, scale.z);

        if(displayList == -1 || true) {
            displayList = GL11.glGenLists(1);
            GL11.glNewList(displayList,GL11.GL_COMPILE_AND_EXECUTE);
            GL11.glBegin(GL11.GL_QUADS);
            // Draw in clockwise - (00,10,11,01); must skip last point of line
            for(i <- 0 until points.length-width-1; if((i+1)%width != 0))
                List(points(i), points(i+1), points(i+width+1), points(i+width)).map(
                    (p:Vec3) => {
                        GL11.glColor3f(p.y/3, p.y*5, p.y/3);
                        GL11.glVertex3f(p.x, p.y, p.z);
                    }
                )
            GL11.glEnd;//*/
            GL11.glEndList;
        } else {
            GL11.glCallList(displayList);
        }
        GL11.glPopMatrix;
    }

    // @make a method to merge quadpatches (for infinite terrain)   
}

// first iter: cylnders for trees
class GenModel extends BasicModel {

    /*def this() {
        this()
        //Displaymodel,transform,displaymodel,... pop matrix
        
    }*/
    
    override def render {
    }
}


class ModelLink(var m1:BasicModel, var m2:BasicModel) extends Vector {
    def this(m1:BasicModel,m2:BasicModel,vector:Vec3) {
        this(m1,m2)
        this.vector = vector;
    }

    private var linked = true;
    def breakLink() = linked = false;
    def forgeLink() = linked = true;
    def isLinked() = linked;
    
    def applyLink() = if(linked) m2.setPosition(m1.pos+vector);
}


