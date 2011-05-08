package AngryPigs

import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.util.glu._
import scala.util._

class Vec3(var x:Float, var y:Float, var z:Float) {
    def this() = this(0f,0f,0f);
    
    def getPoints = List(x,y,z);
    def setPoints(v:Vec3):Vec3 = { this.x=v.x; this.y=v.y; this.z=v.z; this }
    def setPoints(p:List[Float]):Vec3 = setPoints(new Vec3(p(0),p(1),p(2)))
    def setPoints(x:Float,y:Float,z:Float):Vec3 = setPoints(new Vec3(x,y,z))
    
    override def clone = new Vec3(x,y,z);
    def map(f:Float=>Float):Vec3 = { setPoints(getPoints.map(f)); this }
    def applyVector(v:Vec3, multi:Float=1):Vec3 = setPoints(this+(v*multi))
    
    def +(v:Vec3):Vec3 = new Vec3(x+v.x, y+v.y, z+v.z)
    def -(v:Vec3):Vec3 = new Vec3(x-v.x, y-v.y, z-v.z)
    def +=(v:Vec3) = applyVector(v, +1)
    def -=(v:Vec3) = applyVector(v, -1)
    def *(f:Float):Vec3 = this.clone.map(_ * f)
    def X(v:Vec3):Vec3 = new Vec3(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)
    def dot(v:Vec3):Float = (new Vec3(x*v.x, y*v.y, z*v.z)).getPoints.reduceLeft(_+_)
    def length:Float = math.sqrt(this.clone.map(math.pow(_,2).toFloat).getPoints.reduceLeft(_+_)).toFloat
    def ==(v:Vec3):Boolean = (x==v.x && y==v.y && z==v.z)
    def !=(v:Vec3):Boolean = !(this==v)
    
    // clamp values to some value(e.g. world size)
    private def clamp(p:Float,clamp:Float):Float = if(clamp!=0 && math.abs(p) > clamp) clamp*(p/math.abs(p)) else p
    def clamp(c:Float):Vec3 = this.map(clamp(_,c));
    def clamp(cx:Float,cy:Float,cz:Float):Vec3 = setPoints(clamp(x, cx), clamp(y, cy),clamp(z, cz))

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
trait Vector2 {
    var vector2 = new Vec3;
    def setVector2(x:Float,y:Float,z:Float) = { vector2 = new Vec3(x,y,z); }
}
trait Points {
    var points:Array[Vec3]=null;
}

class Quad(var p1:Vec3, var p2:Vec3, var p3:Vec3, var p4:Vec3) {
    def getPoints = List(p1,p2,p3,p4);
    
    def foreach(f:Vec3=>Unit) = getPoints.foreach(f);
    def map(f:Vec3=>Unit) = foreach(f);
}

abstract class BasicModel {
    var (pos,rot,scale) = (new Vec3, new Vec3, new Vec3(1f,1f,1f));

    def setPosition(x:Float,y:Float,z:Float) = { pos = new Vec3(x,y,z); }
    def setPosition(v:Vec3) = { pos = v.clone }
    def setRotation(x:Float,y:Float,z:Float) = { rot = new Vec3(x,y,z); }
    def setRotation(v:Vec3) = { rot = v.clone }
    def setScale(x:Float,y:Float,z:Float) = { scale = new Vec3(x,y,z) }
    def setScale(v:Vec3) = { scale = v.clone }
    
    def doTranslate {
        GL11.glTranslatef(pos.x, pos.y, pos.z);
    }
    def doRotate {
        if(rot.x!=0) GL11.glRotatef(rot.x, 1, 0, 0);        
        if(rot.y!=0) GL11.glRotatef(rot.y, 0, 1, 0);
        if(rot.z!=0) GL11.glRotatef(rot.z, 0, 0, 1);
    }
    def doScaling {
        GL11.glScalef(scale.x, scale.y, scale.z);
    }

    def doTransforms {
        doTranslate
        doRotate
        doScaling
    }

    def render;

    override def toString:String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")";
}

// doesn't care about points and stuff
class DisplayModel(var displayList:Int) extends BasicModel with Vector {
    def this(renderf:Unit=>Unit) {
        this(-1);
        renderfunc = renderf;
        compile;
    }
    var renderfunc:Unit=>Unit = Unit=>{};
    
    def compile {
        displayList = GL11.glGenLists(1);
        GL11.glNewList(displayList,GL11.GL_COMPILE);
        renderfunc();
        GL11.glEndList;        
    }
    
    override def render {
        GL11.glPushMatrix;
        
        if(displayList == -1)
            renderfunc;
        else
            GL11.glCallList(displayList);
        
        GL11.glPopMatrix;
    }
    
}

class QuadPatch(points:Array[Vec3], width:Int) extends BasicModel with Points {
    var displayList = -1
    var compiled: Boolean = false;
    
    def renderfunc {
        GL11.glBegin(GL11.GL_QUADS);
        // Draw in clockwise - (00,10,11,01); must skip last point of line
        for(i <- 0 until points.length-width-1; if((i+1)%width != 0))
            List(points(i), points(i+1), points(i+width+1), points(i+width)).map(
                (p:Vec3) => {
                    //GL11.glColor3f(p.y/3, p.y*5, p.y/3);
                    GL11.glColor3f(0.2f, 0.7f+p.y/2, 0.2f);
                    GL11.glNormal3f(p.y, p.y, p.y);
                    GL11.glVertex3f(p.x, p.y, p.z);
                }
            )
        GL11.glEnd;//*/
    }
    def compile {
        displayList = GL11.glGenLists(1);
        GL11.glNewList(displayList,GL11.GL_COMPILE_AND_EXECUTE);
        renderfunc;
        GL11.glEndList;
        compiled = true;
    }
    
    override def render {
        GL11.glPushMatrix;
        
        if(displayList == -1) {
            if(compiled) //didn't compile right...
                renderfunc;
            else
                compile;
        } else {
            GL11.glCallList(displayList);
        }
        
        GL11.glPopMatrix;
    }

    // @make a method to merge/expand quadpatches (for infinite terrain)   
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
    
    private var lookAtV = new Vec3
    def lookAt(v:Vec3) = lookAtV = v.clone;
    def lookAt(m:BasicModel) = lookAtV = m.pos.clone;
    
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
        
        GLU.gluLookAt(pos.x,pos.y,pos.z,             // camera position
                      lookAtV.x,lookAtV.y,lookAtV.z, // look-at vector
                      0,1,0)                         // up vector 
    }
}

class ModelLink(var m1:BasicModel, var m2:BasicModel) extends Vector with Vector2 {
    def this(m1:BasicModel,m2:BasicModel,vector:Vec3, vector2:Vec3) {
        this(m1,m2)
        this.vector = vector;
        this.vector2 = vector2;
    }
    def this(m1:BasicModel,m2:BasicModel,vector:Vec3) {
        this(m1,m2, vector,new Vec3)
    }

    private var linked = true;
    def isLinked = linked;
    def breakLink { linked = false }
    def forgeLink { linked = true }
    
    def applyLink {
        if(linked) {
            m2.setPosition(m1.pos+vector);
            m2.setRotation(m1.rot+vector2);
        }
    }
}


