package AngryPigs

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.Random.{nextInt,nextFloat}

abstract class Model {
  var (pos,rot,scale) = (Vec3(), Vec3(), Vec3(1f,1f,1f))
  var visible = true

  def setPosition(x: Float, y: Float, z: Float) { pos = Vec3(x,y,z) }
  def setRotation(x: Float, y: Float, z: Float) { rot = Vec3(x,y,z) }
  def setScale(x: Float, y: Float, z: Float) { scale = Vec3(x,y,z) }
  def setPosition(v: Vec3) { pos = v.clone }
  def setRotation(v: Vec3) { rot = v.clone }
  def setScale(v: Vec3) { scale = v.clone }
  
  def doTranslate() {
    GL11.glTranslatef(pos.x, pos.y, pos.z)
  }
  def doRotate() {
    if(rot.z != 0) GL11.glRotatef(rot.z, 0, 0, 1)
    if(rot.y != 0) GL11.glRotatef(rot.y, 0, 1, 0)
    if(rot.x != 0) GL11.glRotatef(rot.x, 1, 0, 0)
  }
  def doScaling() {
    GL11.glScalef(scale.x, scale.y, scale.z)
  }

  def doTransforms() {
    doTranslate()
    doRotate()
    doScaling()
  }

  def render(): Unit

  override def toString: String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")"
}

// doesn't care about points and stuff
class DisplayModel(var renderfunc: () => Unit = () => (), var idfunc: (DisplayModel,SettingMap[String]) => Int = null) extends Model with Properties {
  var compiled = false
  
  var (vector,vector2) = (Vec3(), Vec3())
  var displayList: Int = -1
  
  // foo value
  properties += "graphics" -> -1
  
  def forceCompile() {
    import Global._
    displayList = GL11.glGenLists(1)
    GL11.glNewList(displayList, GL11.GL_COMPILE)
    renderfunc()
    GL11.glEndList()
    compiled = true
    properties += "graphics" -> Settings.graphics
    
    try { id() } catch {
      case e: NullPointerException =>
        compileCache += nextInt -> displayList
        //reset()
    }
  }
  
  def id(props: SettingMap[String] = this.properties): Int = {
    if(idfunc != null) {
      idfunc(this, props) 
    } else { 
      throw new NullPointerException()
    }
  }
  
  val compileCache = new HashMap[Int,Int]
  
  // adds compile cache to compile()
  def compile() {
    try {
      displayList = compileCache.getOrElseUpdate(id(Global.settings), {
        forceCompile()
        displayList
      })
      properties += "graphics" -> Global.Settings.graphics
    } catch {
      case e: NullPointerException => forceCompile()
    }
  }
  
  def reset(limit: Int = 1, preserveCurrent: Boolean = true) {
    if(compileCache.size > limit) {
      var count = 0
      compileCache.clone.foreach {
        case (id,listid) => 
          if(listid != displayList || !preserveCurrent) {
            count += 1
            Global.tasks = Global.tasks :+ (() => GL11.glDeleteLists(listid, 1))
            compileCache -= id
          }
      }
    }
  }
  def free() {
    reset(limit = 0, preserveCurrent = false)
    displayList = -1
  }

  override def clone: DisplayModel = {
    val res = new DisplayModel(this.renderfunc)
    res.pos = this.pos.clone
    res.rot = this.rot.clone
    res.scale = this.scale.clone
    res
  }
  
  override def render() {
    if(!visible) return
    
    GL11.glPushMatrix()
    doTransforms

    if(displayList == -1 || !GL11.glIsList(displayList)) {
      renderfunc()
      displayList = -1
    } else {
      GL11.glCallList(displayList)
    }
      
    GL11.glPopMatrix()
  }
}

class GeneratorModel(generator: () => Object, draw: Object => Unit, _idfunc: (DisplayModel,SettingMap[String]) => Int = null) extends DisplayModel {
  var data: Object = generator()
  renderfunc = () => { draw(data); () }
  idfunc = _idfunc
  
  def regenerate() {
    data = generator()
    compile()
  }
  
  // make a data constructor, so clone has same data. (eliminate generator in static constructor)
  override def clone: GeneratorModel = {
    val res = new GeneratorModel(generator, draw)
    res.pos = this.pos.clone
    res.rot = this.rot.clone
    res.scale = this.scale.clone
    res
  }
}

class TrailModel(var points: List[Vec3]) 
  extends GeneratorModel(
    () => { points.map(_.clone) }, 
    (data: Object) => {
      import org.lwjgl.opengl.GL11._
      import Global._

      val points = data.asInstanceOf[List[Vec3]]
      glColor3f(1f,1f,1f)
      for(i <- 1 until points.length by 2) {
        val (vecA,vecB) = (points(i-1), points(i))
        
        val z = Vec3(0,0,1)
        val p = vecA - vecB
        val cross = z X p
        val angle = z angle p
        
        glPushMatrix()
        glTranslatef(vecB.x,vecB.y,vecB.z)
        glRotatef(angle,cross.x,cross.y,cross.z)
        gluQuadrics.cylinder.draw(0.075f,0.075f, p.length, 4,1)
        glPopMatrix()
      }
    }, 
    (model: DisplayModel, props: SettingMap[String]) => model.asInstanceOf[GeneratorModel].data.asInstanceOf[List[Vec3]].length) {
    
  def +=(v: Vec3) = {
    data = data.asInstanceOf[List[Vec3]] ++ List(v.clone)
    compile()
  }
}

class Branch(var parent: Branch) extends Properties {
  var (diffVec,rootVec) = (Vec3(), Vec3())
  def destVec: Vec3 = rootVec+diffVec

  var depth = 1
  var marked = false
  val children = new ListBuffer[Branch]

  setParent(parent)

  def setParent(p: Branch) {
    if(p != null) {
      p.children += this
      depth = p.depth+1
    }
    parent = p
  }
  def addChild(c: Branch): Unit = if(!(this eq c)) c.setParent(this)
  
  def detach() {
    if(parent != null) {
      parent.children -= this
      this.setParent(null)
    }
  }
  
  def doAll(f: Branch => Unit) {
    f(this)
    children.foreach(_.doAll(f))
  }
  def doWhile(w: Branch => Boolean, f: Branch => Unit) {
    f(this)
    if(w(this)) children.foreach(_.doWhile(w, f))
  }

  def print() {
    println(depth+" "*(depth*2) + rootVec +" -- " + diffVec)
    children.foreach(_.print())
  }
  
  def render() {
    import org.lwjgl.opengl.GL11._
    import Global._
    if(depth <= Settings.maxdepth) {
      val (vecA,vecB) = (rootVec, destVec)
      
      val z = Vec3(0,0,1)
      val p = vecA - vecB
      val cross = z X p
      val angle = z angle p
      
      glPushMatrix()
      glTranslatef(vecB.x,vecB.y,vecB.z)
      glRotatef(angle,cross.x,cross.y,cross.z)
      glColor3f(0.7f,0.2f,0f)
      val fatness = properties.get[Float]("fatness")
      gluQuadrics.cylinder.draw(fatness/depth,(fatness*2)/depth, diffVec.length, Settings.graphics*5,1)
      if(properties.get[Boolean]("hasLeaf")) {
        glDisable(GL_CULL_FACE)
        glScalef(1,1.6f,1)
        glColor3f(0.2f,0.8f,0.1f)
        glTranslatef(0,-0.17f,0)
        glRotatef(nextFloat*12-nextFloat*12, 0,0,1)
        gluQuadrics.disk.draw(0,0.175f, Settings.graphics*6,1)
        glEnable(GL_CULL_FACE)
      }
      glPopMatrix()
    }
  }
}

class Camera extends Model {
  // default projection 
  var perspective = false
  var (near,far) = (1f,30f) // near, far clipping plane
  var (fov,aspectRatio) = (45f,4/3f) // perspective stuff
  var (minX,minY,maxX,maxY) = (-1f,-1f, 1f, 1f) // ortho stuff
  var projectionChanged = true // do we need to remake projection matrix
  var vector = Vec3()
  var angle = Vec3()

  // set a perspective projection
  def setPerspective(fv: Float, ar: Float, n: Float, f: Float) {
    perspective = true
    fov = fv
    aspectRatio = ar
    near = n
    far = f
    projectionChanged = true
  }
  
  // set an ortographic projection
  def setOrtho(mx: Float, my: Float, Mx: Float, My: Float, n: Float, f: Float) {
    perspective = false
    minX = mx
    minY = my
    maxX = Mx
    maxY = My
    near = n
    far = f
    projectionChanged = true
  }
  
  private var lookAtV = Vec3()
  def lookAt(v: Vec3): Unit = lookAtV = v.clone
  def lookAt(m: Model): Unit = lookAtV = m.pos.clone
    
  override def render() {
    // setup projection matrix stack
    if(projectionChanged) {
      projectionChanged = false
      GL11.glMatrixMode(GL11.GL_PROJECTION)
      GL11.glLoadIdentity()
      if(perspective) {
        // perspective projection
        GLU.gluPerspective(fov,aspectRatio, near,far)
      } else {
        // orthographic projection 
        GL11.glOrtho(minX,maxX, minY,maxY, near,far)
      }
    }

    // model view stack 
    GL11.glMatrixMode(GL11.GL_MODELVIEW)
    GL11.glLoadIdentity()
    GLU.gluLookAt(pos.x,pos.y,pos.z,       // camera position
                  lookAtV.x,lookAtV.y,lookAtV.z, // look-at vector
                  0,1,0)             // up vector 
  }
}

class ModelLink(m1: Model, m2: Model, var vector: Vec3=Vec3(), var vector2: Vec3=Vec3()) {
  private var linked = false
  def isLinked: Boolean = linked
  def breakLink() { linked = false }
  def forgeLink() { linked = true }
  
  def applyLink() {
    if(linked) {
      m1.setPosition(m2.pos+vector)
      m1.setRotation(m2.rot+vector2)
    }
  }
}
