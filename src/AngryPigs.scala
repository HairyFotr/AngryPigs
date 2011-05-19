package AngryPigs

import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import org.lwjgl.util.vector._
import scala.util.Random
import scala.collection.mutable._
import java.util.{List => JavaList}
import clojure.lang._
import clojure.core._
import java.nio._

// TODO:
// search for @ <task>

object Game {
    import Global._
    import org.lwjgl.opengl.GL11._

    var isRunning = false; // is main loop running
    //val acceptModes
    var (winWidth, winHeight)=(3000,3000); // window size
    var renderTime=0f;
    val cam = new Camera;
    
    val timeLock = new TimeLock;
    val pauseLock = new TimeLock;
    
    /**
     * Initializes display and enters main loop
     */
    def main(Args:Array[String]) {
        try {
          initDisplay;
        } catch {
            case e:Exception => {
              print("Can't open display. "+e.getMessage);
              exit(1);
            }
        }

        // enter loop
        isRunning = true;
        mainLoop();
        
        // cleanup
        Display.destroy();
    }

    def initDisplay {
        var bestMode:DisplayMode = null;
        val modes:Array[DisplayMode] = Display.getAvailableDisplayModes;
        // Get best mode
        for(mode <- modes)
            if((mode.getWidth <= winWidth && mode.getHeight <= winHeight && mode.getFrequency <= 100)
                &&(bestMode == null
                   ||(mode.getWidth >= bestMode.getWidth && mode.getHeight >= bestMode.getHeight
                      && mode.getBitsPerPixel >= bestMode.getBitsPerPixel
                      && mode.getFrequency >= bestMode.getFrequency)))
                bestMode = mode;
        
        Display.setDisplayMode(bestMode);
        winWidth = bestMode.getWidth;
        winHeight = bestMode.getHeight;
        
        println("Display: "+bestMode.getWidth+"x"+bestMode.getHeight+"@"+bestMode.getFrequency+"Hz, "+bestMode.getBitsPerPixel+"bit")
        
        try {
            // FSAA
            Display.create(new PixelFormat(8, 16, 0, 1));
        } catch {
            // No FSAA            
            case _ =>
                Display.create;
        }
        Display.setTitle("Angry Pigs");
        Display.setVSyncEnabled(true);
    }

    def now = System.nanoTime();
    // used for frame independant movement
    var frameTime = now;
    
    /**
     * Main loop: renders and processes input events
     */
    def mainLoop() { 
        //loadModels; // load models
        makeModels; // make generative models
        setupView;  // setup camera and lights
    
        frameTime = now;
        // @that is one ugly FPS counter :)
        var tenSecondTimer = now;
        var frameCounter = 0;
        val E10 = 10000000000L;
        while(isRunning) {
            // @menda se da sproti/bolš gledat input
            processInput // process input events 
            if(pause) Thread.sleep(50)
            
            resetView;      // clear view and reset transformations
            renderFrame;    // draw stuff
            Display.update; // update window contents and process input messages
            frameCounter += 1;

            //gl error
            val errCode = glGetError;
            if (errCode != GL_NO_ERROR) 
                println(opengl.Util.translateGLErrorString(errCode));

            if(now-tenSecondTimer > E10) {
                tenSecondTimer = now;
                // print fps
                println("FPS: "+frameCounter/10);

                if((frameCounter/10) < 30 && settings.getFloat("graphics") > 1f) {
                    settings += "graphics" -> (settings.getFloat("graphics")-1f);
                    println("decreased graphic detail");
                    compiledmodels.map(_.compile);
                }
                if((frameCounter/10) > 50 && settings.getFloat("graphics") < 10f) {
                    settings += "graphics" -> (settings.getFloat("graphics")+1f);
                    println("inreased graphic detail");
                    compiledmodels.map(_.compile);
                }

                frameCounter = 0;
            }

            renderTime = (now-frameTime)/frameIndepRatio;
            frameTime = now; 
        }
    }
    
    //models
    var terrain:GeneratorModel=null;
    var skybox:DisplayModel=null;
    var coordsys:DisplayModel=null;
    var pig:DisplayModel=null;
    var catapult:DisplayModel=null;
    var trees = new ListBuffer[GeneratorModel];
    var pigcatapultLink:ModelLink=null;
    var campigLink:ModelLink=null;
    var compiledmodels = new ListBuffer[DisplayModel];
    
    //size of world
    val worldSize = 400;
    val gravity = new Vec3(0f,-0.5f,0f);
    
    // @would it pay-off to make model generation lazy and generate them on the fly?
    // @infinite terrain patches and stuff
    def makeModels {
        // terrain
        val detail=40;
        val height=0.3f;
        
        def genTerrain:()=>Object = ()=>{
            def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
            val p = (for(i <- 0 to detail; j <- 0 to detail) yield getTerrainPoint(i,j)).toArray;
            p;
        }
        def drawTerrain = (data:Object)=>{
            val points = data.asInstanceOf[Array[Vec3]];
            GL11.glBegin(GL11.GL_QUADS);
            // Draw in clockwise - (00,10,11,01); must skip last point of line
            val width = math.sqrt(points.length).toInt;
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
            ();
        }
        
        terrain = new GeneratorModel(genTerrain, drawTerrain);
        terrain.setPosition(-worldSize,-worldSize,-worldSize);
        terrain.setScale(worldSize*2, 5, worldSize*2);
        
        // coordinate system
        coordsys = new DisplayModel(()=>{
            glBegin(GL_LINES);
                glColor3f(1,0,0);
                glVertex3f(0,0,0);
                glVertex3f(1,0,0);
                
                glColor3f(0,1,0);
                glVertex3f(0,0,0);
                glVertex3f(0,1,0);
                
                glColor3f(0,0,1);
                glVertex3f(0,0,0);
                glVertex3f(0,0,1);
            glEnd;//*/
        });
        coordsys.setScale(worldSize,worldSize,worldSize);

        // sky-box
        skybox = new DisplayModel(()=>{
            glBegin(GL_QUADS);
                glColor3f(0.5f,0.5f,0.5f);
                // top
                //glColor3f(0f,1f,0f); // green
                glVertex3f( 2f, 1f,-2f);
                glVertex3f(-2f, 1f,-2f);
                glVertex3f(-2f, 1f, 2f);
                glVertex3f( 2f, 1f, 2f);
                // bottom 
                /*glColor3f(1f,0.5f,0f);    // orange
                glVertex3f( 1f,-1f, 1f);
                glVertex3f(-1f,-1f, 1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f( 1f,-1f,-1f);*/
                // front
                //glColor3f(1f,0f,0f); // red 
                glVertex3f( 2f, 1f, 2f);
                glVertex3f(-2f, 1f, 2f); 
                glVertex3f(-1f,-1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                // back

                //glColor3f(1f,1f,0f); // yellow
                glVertex3f( 1f,-1f,-1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-2f, 1f,-2f);
                glVertex3f( 2f, 1f,-2f);
                // left
                //glColor3f(0f,0f,1f); // blue
                glVertex3f(-2f, 1f, 2f);
                glVertex3f(-2f, 1f,-2f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-1f,-1f, 1f);
                // right
                //glColor3f(1f,0f,1f); // violet
                glVertex3f( 2f, 1f,-2f);
                glVertex3f( 2f, 1f, 2f);
                glVertex3f( 1f,-1f, 1f);
                glVertex3f( 1f,-1f,-1f);
            glEnd;
        });
        skybox.setPosition(0,0,0);
        skybox.setScale(worldSize,worldSize,worldSize);//*/

        // pig
        pig = new DisplayModel(()=>{
            //body
            glColor3f(0.3f,0.8f,0.3f);
            glPushMatrix;
            {
                glScalef(0.95f,1,1.05f);
                gluQuadrics.sphere.draw(2,(settings.getFloat("graphics")*15f).toInt,(settings.getFloat("graphics")*15f).toInt);
            }
            glPopMatrix
            //ears
            glColor3f(0.4f,0.9f,0.4f);
            glPushMatrix;
            {
                val x = 0.9f;
                glRotatef(180,0,1,0)
                glTranslatef(x,1.7f,-0.7f);
                gluQuadrics.disk.draw(0,0.35f, (settings.getFloat("graphics")*8f).toInt,1);
                glTranslatef(-2*x,0,0);
                gluQuadrics.disk.draw(0,0.35f, (settings.getFloat("graphics")*8f).toInt,1);
            }
            glPopMatrix
            //nose            
            glColor3f(0.4f,1f,0.4f);
            glPushMatrix;
            {
                glScalef(1,1,1);
                //glRotatef(90, 0,1,0)
                glTranslatef(0,0.4f,1.4f);
                val size=0.7f
                gluQuadrics.cylinder.draw(size,size, 1, (settings.getFloat("graphics")*10f).toInt,1);
                glTranslatef(0,0,1);
                gluQuadrics.disk.draw(0,size, (settings.getFloat("graphics")*10f).toInt,1);
            }
            //moustache
            //@make swizec make a moustache generator :P
            if(rand.nextFloat > 0.2) {
                glScalef(2,1,1);
                glColor3f(0.7f,0.2f,0f);
                if(rand.nextFloat > 0.2) {
                    glTranslatef(0,-0.7f,-0.2f)
                    gluQuadrics.disk.draw(0,0.5f, (settings.getFloat("graphics")*10f).toInt,1);
                } else {
                    glTranslatef(0,-0.8f,-0.3f)
                    gluQuadrics.partialdisk.draw(0,0.5f, (settings.getFloat("graphics")*10f).toInt,1, 270, 180);
                }
            }
            glPopMatrix
            //eyes
            glPushMatrix;
            {
                val x = 1.2f;
                val glasses = if(rand.nextFloat > 0.5) true else false

                def drawEye = {
                    glPushMatrix;
                    glColor3f(0.8f,0.8f,0.8f);
                    gluQuadrics.sphere.draw(0.5f,(settings.getFloat("graphics")*8f).toInt,(settings.getFloat("graphics")*8f).toInt);
                    val z = 0.35f;
                    glTranslatef(0,0,z);
                    glColor3f(0.1f,0.1f,0.1f);
                    gluQuadrics.sphere.draw(0.25f,(settings.getFloat("graphics")*8f).toInt,(settings.getFloat("graphics")*8f).toInt);
                    if(glasses) {
                        glTranslatef(0,0,0.1f);
                        gluQuadrics.disk.draw(0.67f,0.77f, (settings.getFloat("graphics")*8f).toInt,(settings.getFloat("graphics")*8f).toInt);
                    }
                    glPopMatrix
                }
                glTranslatef(x,0.6f,1.2f);
                drawEye;
                glTranslatef(-2*x,0,0);
                drawEye;
            }
            glPopMatrix
        });
        pig.setPosition(0,-worldSize+7f,-worldSize/2+25);
        pig.compile();

        catapult = new DisplayModel(()=>{
            val scale = new Vec3(4f,1f,6.5f)
            glPushMatrix;
            glScalef(scale.x,scale.y,scale.z);
            glColor3f(0.8f,0.3f,0f);
            glBegin(GL_QUADS);                
                // top
                glNormal3f( 0f, 1f, 0f);
                glVertex3f( 1f, 1f,-1f);
                glVertex3f(-1f, 1f,-1f);
                glVertex3f(-1f, 1f, 1f);
                glVertex3f( 1f, 1f, 1f);
                // bottom 
                glNormal3f( 0f,-1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                glVertex3f(-1f,-1f, 1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f( 1f,-1f,-1f);
                // Front
                glNormal3f( 0f, 0f, 1f);
                glVertex3f( 1f, 1f, 1f);
                glVertex3f(-1f, 1f, 1f); 
                glVertex3f(-1f,-1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                // back
                glNormal3f( 0f, 0f,-1f);
                glVertex3f( 1f,-1f,-1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-1f, 1f,-1f);
                glVertex3f( 1f, 1f,-1f);
                // left
                glNormal3f(-1f, 0f, 0f);
                glVertex3f(-1f, 1f, 1f);
                glVertex3f(-1f, 1f,-1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-1f,-1f, 1f);
                // right
                glNormal3f( 1f, 0f, 0f);
                glVertex3f( 1f, 1f,-1f);
                glVertex3f( 1f, 1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                glVertex3f( 1f,-1f,-1f);
            glEnd;
            glPopMatrix;

            def drawWheel = {
                glRotatef(90, 0,1,0)
                gluQuadrics.cylinder.draw(1f,1f, scale.x*2+2, (settings.getFloat("graphics")*10f).toInt,1);
                gluQuadrics.disk.draw(0,1,20,1);
                glTranslatef(0,0,scale.x*2+2);
                gluQuadrics.disk.draw(0,1,20,1);
            }
            // Front wheel
            glPushMatrix;
            glTranslatef(-scale.x-1,-1,scale.z-2f)
            drawWheel
            glPopMatrix;
            // Back wheel
            glPushMatrix;
            glTranslatef(-scale.x-1,-1,-scale.z+2f)
            drawWheel
            glPopMatrix;
        });
        catapult.setPosition(0,-worldSize+2.5f,-worldSize/2+25);
        catapult.compile();
        
        pigcatapultLink = new ModelLink(catapult, pig, new Vec3(0f,2.5f,0f));
        campigLink = new ModelLink(pig, cam, new Vec3(0f,7,-50), new Vec3(0,0,0));
        
        def giveMeTree:()=>Object = ()=>genTree/("give-me-tree", 0f, 2f, 0f, 5f);
        
        def renderTree:Object=>Unit = (data:Object)=>{
            import org.lwjgl.opengl.GL11._
            import Global._
            
            var depth=0;
            //this ugly makes below code a little less ugly
            //@checkout: import scala.collection.JavaConversions._
            def isJavaList(a:Object):Boolean = a.isInstanceOf[JavaList[Object]]
            def asArray(a:Object):Array[Object] = a.asInstanceOf[JavaList[Object]].toArray;

            def drawTree(v:Array[Float], a:Array[Object]):Array[Float] = {
                if(a.length==4 && !isJavaList(a(0))) {
                    val vector = a.toList.map(_.toString.toFloat).toArray;
                    val vec = if(v == null) Array[Float](0,0,0,1) else v
                    
                    if(settings.getBoolean("fattrees")) {
                        val vecA = new Vec3(vec(0)*vec(3),
                                             vec(1)*vec(3),
                                             vec(2)*vec(3))
                                            
                        val vecB = new Vec3(vec(0)*vec(3) + vector(0)*vector(3),
                                             vec(1)*vec(3) + vector(1)*vector(3),
                                             vec(2)*vec(3) + vector(2)*vector(3))
                                            
                        val z = new Vec3(0,0,1)
                        val p = vecA - vecB
                        val cross = z X p
                        val angle = z angle p

                        glPushMatrix
                        glTranslatef(vecB.x,vecB.y,vecB.z);
                        glRotatef(angle,cross.x,cross.y,cross.z);
                        glColor3f(0.7f,0.2f,0f);
                        if(depth==1)
                            gluQuadrics.cylinder.draw(0.2f/depth,0.4f/depth,  vector(1)*vector(3), (settings.getFloat("graphics")*5f).toInt,1);
                        else
                            gluQuadrics.cylinder.draw(0.2f/(depth-1),0.4f/(depth-1),  vector(3), (settings.getFloat("graphics")*5f).toInt,1);
                            
                        if(rand.nextFloat < 0.075 * depth) {
                            glScalef(1,1.6f,1)
                            glTranslatef(0,-0.2f,0)
                            glColor3f(0.2f,0.8f,0.1f);
                            gluQuadrics.disk.draw(0,0.175f, (settings.getFloat("graphics")*5f).toInt,1);
                        }
                        glPopMatrix
                    } else {
                        glColor3f(0.7f,0.2f,0f);
                        glBegin(GL_LINES)
                        glVertex3f(vec(0)*vec(3),
                                   vec(1)*vec(3),
                                   vec(2)*vec(3));
                        
                        glVertex3f(vec(0)*vec(3) + vector(0)*vector(3),
                                   vec(1)*vec(3) + vector(1)*vector(3),
                                   vec(2)*vec(3) + vector(2)*vector(3))
                        glEnd
                    }

                    //vector.toList.foreach(println)
                    //println(depth)
                         
                    return (for(i <- 0 to 3) yield if(i==3) 1f else vec(i)*vec(3) + vector(i)*vector(3)).toArray;
                } else {
                    depth += 1;
                    if(a.length==1 || !isJavaList(asArray(a(1)).apply(0)))
                        for(i <- 0 until a.length) drawTree(v, asArray(a(i)))
                    else 
                        for(i <- 1 until a.length) drawTree(drawTree(v, asArray(a(0))), asArray(a(i)))
                    depth -= 1;

                    // gotta return something...
                    return v;
                }
            }
            drawTree(null, asArray(data));
            ();
        }
        
        var tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(0,-worldSize+2.5f,-worldSize/2+30);
        trees += tree
        
        tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(17,-worldSize+2.5f,-worldSize/2+30);
        trees += tree

        tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(-17,-worldSize+2.5f,-worldSize/2+30);
        trees += tree
        /*
        val (dx,dz) = (17, 11);
        for(i <- 0 until 5) {
            val t = tree.clone;
            def pm = if(rand.nextFloat > 0.5) 1 else -1;
            val vec = new Vec3(dx*pm*i, 0, dz*pm*i)
            t.pos += vec
            trees += t
        }
        //*/
        compiledmodels = compiledmodels ++ List(
            //terrain,
            //skybox,
            //coordsys,
            pig,
            catapult        
        );
        compiledmodels = compiledmodels ++ trees;
    }
    
    //@ Y is this not in some LWJGL lib, if it's really needed?
    def allocFloats(floatarray:Array[Float]):FloatBuffer = {
        val fb:FloatBuffer = ByteBuffer.allocateDirect(floatarray.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
        fb.put(floatarray).asInstanceOf[FloatBuffer].flip();
        return fb;
    }
    
    /**
    * Initial setup of projection of the scene onto screen, lights etc.
    */
    def setupView {
        glEnable(GL_DEPTH_TEST); // enable depth buffer (off by default)
        //glEnable(GL_CULL_FACE);  // enable culling of back sides of polygons
        //glCullFace(GL_BACK);
      
        // smooth shading - Gouraud
        glShadeModel(GL_SMOOTH);
        //glShadeModel(GL_FLAT);

        // lights
        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);

        glLight(GL_LIGHT0, GL_AMBIENT, allocFloats(Array[Float](0.3f, 0.3f, 0.3f, 0.0f)));
        glLight(GL_LIGHT0, GL_DIFFUSE, allocFloats(Array[Float](0.7f, 0.7f, 0.7f, 0.0f)));
        glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 20f);
        glLight(GL_LIGHT0, GL_POSITION, allocFloats(Array[Float](0f, 0f, 10f, 0f)));
        glEnable(GL_COLOR_MATERIAL)
        glMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, allocFloats(Array[Float](0.9f, 0.9f, 0.9f, 0f)));
        glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE );
        
        glViewport(0,0, winWidth,winHeight); // mapping from normalized to window coordinates
       
        glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
        cam.setPerspective(50, winWidth/winHeight.toFloat, 1f, 600f);
        cam.setPosition(0,worldSize-5,-worldSize+5);
        cam.setRotation(0,0,0);
    }
  
    /**
    * Resets the view of current frame
    */
    def resetView {
        // clear color and depth buffer
        glClearColor(0.3f,0.6f,0.8f,1f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity;
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;
    }
  
    var frameIndepRatio = (20000000f);
    var treeView = false;
    var pause = false

    def moveObj = if(treeView) trees(0) else { if(pigcatapultLink.isLinked) catapult else pig };
    
    /**
    * Renders current frame
    */
    def renderFrame {       
        val models = ListBuffer(
            //cam,
            //coordsys,
            terrain,
            //skybox,
            pig,
            catapult
            //tree
        )
        if(!pause) {
            // move pig or catapult
            moveObj.vector.z -= 0.05f*moveObj.vector.z*renderTime;
            moveObj.vector.clamp(0,0,8);

            pig.vector += gravity*renderTime

            val moveVector = new Vec3(
                math.sin(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z,
                moveObj.vector.y,
                math.cos(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z
            )
            moveObj.pos += moveVector*renderTime;
            moveObj.pos.clamp(worldSize-2.5f);
            
            pigcatapultLink.applyLink;
            campigLink.applyLink;
            //cam.pos.clamp(worldSize-5);
        }
                
        //set projection and reset modelview
        //look at this pig... look!
        cam.lookAt(moveObj)
        cam.render

        for(model <- models) {
            glPushMatrix;
            model.doTransforms
            model.render
            glPopMatrix;
        }
        
        for(tree <- trees) {
            glPushMatrix;
            tree.doTransforms
            tree.render
            glPopMatrix;
        }
    }
    
    def processInput {
        import Keyboard._ // static imports.. fuckyeah
        
        if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) isRunning = false;
                
        if(isKeyDown(KEY_T) && !timeLock.isLocked) {
            treeView = !treeView
            timeLock.lockIt(500);
        }
        if(isKeyDown(KEY_1)) { settings += "fattrees" -> false; trees.toList.map(_.compile()) }
        if(isKeyDown(KEY_2)) { settings += "fattrees" -> true; trees.toList.map(_.compile()) }
        if(isKeyDown(KEY_3)) { trees.toList.map(_.regenerate()) }
        if(isKeyDown(KEY_5)) { 
            settings += "graphics" -> (settings.getFloat("graphics")+1f);
            compiledmodels.map(_.compile()) 
        }
        if(isKeyDown(KEY_6) && settings.getFloat("graphics") > 1) { 
            settings += "graphics" -> (settings.getFloat("graphics")-1f);
            compiledmodels.map(_.compile()) 
        }
        if(isKeyDown(KEY_9) && !timeLock.isLocked) { pig.compile; timeLock.lockIt(300); }
        
        val keymove = 0.7f*renderTime;
        
        if(campigLink.isLinked) {
            if(isKeyDown(KEY_Q)) campigLink.vector2.x+=keymove;
            if(isKeyDown(KEY_E)) campigLink.vector2.x-=keymove;
            if(isKeyDown(KEY_A)) campigLink.vector2.y+=keymove;
            if(isKeyDown(KEY_D)) campigLink.vector2.y-=keymove;
            if(isKeyDown(KEY_Y)) campigLink.vector2.z+=keymove;
            if(isKeyDown(KEY_C)) campigLink.vector2.z-=keymove;

            if(isKeyDown(KEY_W)) campigLink.vector.x+=keymove;
            if(isKeyDown(KEY_R)) campigLink.vector.x-=keymove;
            if(isKeyDown(KEY_S)) campigLink.vector.y+=keymove;
            if(isKeyDown(KEY_F)) campigLink.vector.y-=keymove;
            if(isKeyDown(KEY_X)) campigLink.vector.z+=keymove;
            if(isKeyDown(KEY_V)) campigLink.vector.z-=keymove;
        } else {
            if(isKeyDown(KEY_W)) cam.pos.x+=keymove;
            if(isKeyDown(KEY_R)) cam.pos.x-=keymove;
            if(isKeyDown(KEY_S)) cam.pos.y+=keymove;
            if(isKeyDown(KEY_F)) cam.pos.y-=keymove;
            if(isKeyDown(KEY_X)) cam.pos.z+=keymove;
            if(isKeyDown(KEY_V)) cam.pos.z-=keymove;
        }

        if(isKeyDown(KEY_P)) {
            pause = true;
            println("paused")
        }
        if(isKeyDown(KEY_RSHIFT)) {
            pause = false;
            println("unpaused")
        }
        if(pause) return;

        if(isKeyDown(KEY_LEFT))  moveObj.rot.y+=keymove*3f;
        if(isKeyDown(KEY_RIGHT)) moveObj.rot.y-=keymove*3f;
        if(isKeyDown(KEY_UP))    moveObj.vector.z+=keymove/5f;
        if(isKeyDown(KEY_DOWN))  moveObj.vector.z-=keymove/5f;
        
        if(isKeyDown(KEY_SPACE) && !timeLock.isLocked){
            if(pigcatapultLink.isLinked) {
                pigcatapultLink.breakLink;
                println("pig-catapult Link Broken")
                campigLink.breakLink
                println("cam-pig Link Broken")
                pig.vector.y=5f;
                pig.vector.z=8f;
            } else {
                pig.vector.y=3f;
                pig.vector.z=4f;
            }
            timeLock.lockIt(1000);
        }
        if(isKeyDown(KEY_LCONTROL) && !pigcatapultLink.isLinked) {
            pigcatapultLink.forgeLink;
            println("pig-catapult Link Forged")
            campigLink.forgeLink
            println("cam-pig Link Forged")
        }
        
        if(isKeyDown(KEY_O)) {
            println("Cam: "+cam.toString);
            println("Pig: "+pig.toString);
        }
        if(isKeyDown(KEY_8) && campigLink.isLinked) {
            campigLink.breakLink
            println("cam-pig Link Broken")
        }
        if(isKeyDown(KEY_7) && !campigLink.isLinked) {
            campigLink.forgeLink
            println("cam-pig Link Forged")
        }
        if(isKeyDown(KEY_0)) {
            cam.setPosition(0,worldSize-5,-worldSize+5);
            cam.setRotation(0,180,0);
            println("cam Reset")
        }
    }
}



