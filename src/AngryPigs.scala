package AngryPigs

import org.lwjgl.opengl.{Display,PixelFormat,DisplayMode,Util}
import org.lwjgl.input.Keyboard
import scala.collection.mutable.ListBuffer
import java.util.{List => JavaList}
import java.nio._

// TODO:
// search for @ <task>

object Game {
    import Global._
    import org.lwjgl.opengl.GL11._

    var isRunning = false; // is main loop running
    var (winWidth, winHeight)=(3000,3000); // window size
    var renderTime=0f;
    
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
    
        // FPS counter
        var frameCounter = 0
        val second = 1000000000L
        val FPSseconds = 5 
        var FPStimer = now
        frameTime = now
        while(isRunning) {
            processInput // process keyboard input
            if(pause) Thread.sleep(50)
            
            resetView;      // clear view and reset transformations
            renderFrame;    // draw stuff
            Display.update; // update window contents and process input messages
            frameCounter += 1;

            if(now-FPStimer > second*FPSseconds) {
                val FPS = frameCounter/FPSseconds.toFloat;
                println("FPS: "+FPS);

                // increase or decrease graphics detail
                if(FPS < 15 && settings.get[Float]("graphics") > 1f) {
                    settings += "graphics" -> (settings.get[Float]("graphics")-1f);
                    println("decreased graphic detail");
                    models.foreach(_.compile);
                }
                if(FPS > 50 && settings.get[Float]("graphics") < 10f) {
                    settings += "graphics" -> (settings.get[Float]("graphics")+1f);
                    println("inreased graphic detail");
                    models.foreach(_.compile);
                }

                frameCounter = 0;
                FPStimer = now;
            }

            renderTime = (now-frameTime)/frameIndepRatio;
            frameTime = now;
        }
    }
    
    //models
    val cam = new Camera;
    var terrain:GeneratorModel with Properties=null;
    var skybox:DisplayModel=null;
    var coordsys:DisplayModel=null;
    var pig:GeneratorModel=null;
    var catapult:DisplayModel=null;
    var trees = new ListBuffer[GeneratorModel];
    var pigcatapultLink:ModelLink=null;
    var campigLink:ModelLink=null;
    var models = new ListBuffer[DisplayModel];
    var generatedModels = new ListBuffer[GeneratorModel];
    var dropBranches = new ListBuffer[GeneratorModel];
    var trails = new ListBuffer[TrailModel];
    
    //size of world
    val worldSize = 400;
    val gravity = new Vec3(0f,-0.5f,0f);
    
    // @would it pay-off to make model generation lazy and generate them on the fly?
    // @infinite terrain patches and stuff
    def makeModels {
        // terrain
        val detail=30;
        val height=0.3f;
        
        def genTerrain:()=>Object = ()=>{
            def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
            val p = (for(i <- 0 to detail; j <- 0 to detail) yield getTerrainPoint(i,j)).toArray;
            p;
        }
        def drawTerrain = (data:Object)=>{
            val points = data.asInstanceOf[Array[Vec3]];
            glBegin(GL_QUADS);
            // Draw in clockwise - (00,10,11,01); must skip last point of line
            val width = math.sqrt(points.length).toInt;
            for(i <- 0 until points.length-width-1; if((i+1)%width != 0))
                List(points(i), points(i+1), points(i+width+1), points(i+width)).foreach(
                    (p:Vec3) => {
                        //glColor3f(p.y/3, p.y*5, p.y/3);
                        glColor3f(0.2f, 0.7f+p.y/2, 0.2f);
                        glNormal3f(p.y, p.y, p.y);
                        glVertex3f(p.x, p.y, p.z);
                    }
                )
            glEnd;//*/
            ();
        }
        
        terrain = new GeneratorModel(genTerrain, drawTerrain) with Properties;
        terrain.setPosition(-worldSize,-worldSize,-worldSize);
        terrain.setScale(worldSize*2, 5, worldSize*2);
        terrain.compile();
        terrain.properties += "visible"->true;
        
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
        
        def genPig:()=>Object = ()=>{
            var pigData = new SettingMap[String];
            pigData += "Moustache.has" -> (rand.nextFloat > 0.2)
            pigData += "Moustache.which" -> (rand.nextInt(2))
            pigData += "Glasses.has" -> (rand.nextFloat > 0.2)
        }
        
        def drawPig(data:Object):Unit = {
            var pigData = data.asInstanceOf[SettingMap[String]];
            val graphics = settings.get[Float]("graphics");
            //body
            glColor3f(0.3f,0.8f,0.3f);
            glPushMatrix;
            {
                glScalef(0.95f,1,1.05f);
                gluQuadrics.sphere.draw(2,(graphics*16f).toInt,(graphics*16f).toInt);
            }
            glPopMatrix
            //ears
            glColor3f(0.4f,0.9f,0.4f);
            glPushMatrix;
            {
                val x = 0.9f;
                glRotatef(180,0,1,0)
                glTranslatef(x,1.7f,-0.7f);
                gluQuadrics.disk.draw(0,0.35f, (graphics*8f).toInt,1);
                glTranslatef(-2*x,0,0);
                gluQuadrics.disk.draw(0,0.35f, (graphics*8f).toInt,1);
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
                gluQuadrics.cylinder.draw(size,size, 1, (graphics*12f).toInt,1);
                glTranslatef(0,0,1);
                gluQuadrics.disk.draw(0,size, (graphics*12f).toInt,1);
            }
            //moustache
            if(pigData.get[Boolean]("Moustache.has")) {
                glScalef(2,1,1);
                glColor3f(0.7f,0.2f,0f);
                pigData.get[Int]("Moustache.which") match {
                    case 0 =>                
                        glTranslatef(0,-0.7f,-0.2f)
                        gluQuadrics.disk.draw(0,0.5f, (graphics*9f).toInt,1);
                    case 1 =>
                        glTranslatef(0,-0.8f,-0.3f)
                        gluQuadrics.partialdisk.draw(0,0.5f, (graphics*9f).toInt,1, 270, 180);
                    case _ =>
                        glTranslatef(0,-0.8f,-0.3f)
                        gluQuadrics.partialdisk.draw(0,0.5f, (graphics*9f).toInt,1, 270, 180);
                }
            }
            glPopMatrix
            //eyes
            glPushMatrix;
            {
                val x = 1.2f;
                def drawEye = {
                    glPushMatrix;
                    glColor3f(0.8f,0.8f,0.8f);
                    gluQuadrics.sphere.draw(0.5f,(graphics*8f).toInt,(graphics*8f).toInt);
                    val z = 0.35f;
                    glTranslatef(0,0,z);
                    glColor3f(0.1f,0.1f,0.1f);
                    gluQuadrics.sphere.draw(0.25f,(graphics*8f).toInt,(graphics*8f).toInt);
                    if(pigData.get[Boolean]("Glasses.has")) {
                        glTranslatef(0,0,0.1f);
                        gluQuadrics.disk.draw(0.67f,0.77f, (graphics*10f).toInt,1);
                    }
                    glPopMatrix
                }
                glTranslatef(x,0.6f,1.2f);
                drawEye;
                glTranslatef(-2*x,0,0);
                drawEye;
            }
            glPopMatrix
        }        
        
        pig = new GeneratorModel(genPig, drawPig);
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
                gluQuadrics.cylinder.draw(1f,1f, scale.x*2+2, (settings.get[Float]("graphics")*10f).toInt,1);
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
        
        def giveMeTree:()=>Object = ()=>{
            // OH THE HUGE MANATEEE!
            import java.util.{List=>JavaList}
            
            def isJavaList(a:Object):Boolean = a.isInstanceOf[JavaList[Object]]
            def asArray(a:Object):Array[Object] = a.asInstanceOf[JavaList[Object]].toArray;
            def asFloatArray(a:Array[Object]):Array[Float] = {a.toList.map((a)=>{
                if(a.isInstanceOf[java.lang.Double])
                    a.asInstanceOf[java.lang.Double].floatValue();
                else
                    a.asInstanceOf[Float]
            }).toArray}

            def traverse(data:Array[Object], parent:Branch=null):Branch = {
                if(data.length==1) { // unpack thingy ... ((...))
                    traverse(asArray(data(0)), parent)
                } else if(data.length==4 && !isJavaList(data(0))) { // leaves ... (node)
                    val vector = asFloatArray(data);
                    var res = new Branch(parent);
                    if(parent!=null) res.rootVec = parent.rootVec+parent.diffVec;                        
                    res.diffVec = (new Vec3(vector(0), vector(1), vector(2))) * vector(3)
                    res.properties += (if(rand.nextFloat < 0.075*res.depth) "hasLeaf"->true else "hasLeaf"->false);
                    res;
                } else if(!isJavaList(asArray(data(0)).apply(0)) && isJavaList(asArray(data(1)).apply(0))) { // parent & subbranches ((node) (...))
                    var newparent = traverse(asArray(data(0)), parent);
                    for(i <- 1 until data.length) traverse(asArray(data(i)), newparent);
                    newparent;
                } else { // branches ... ((...) (...) (...))
                    for(i <- 0 until data.length) traverse(asArray(data(i)), parent);
                    parent;
                }
            }
            
            var data:Object = null;
            while(data==null) try {
                data = genTree/("give-me-tree", 0f, 2f, 0f, 5f);
            } catch {
                case e:RuntimeException => {
                    println("give-me-tree threw exception");                    
                    e.printStackTrace;
                    data = null;
                }
            }
            
            val tree = traverse(asArray(data));
            
            def generateBoxes(branch:Branch):BoundingBox = {
                var box = new BoundingBox(List(branch.rootVec, branch.destVec));
                
                for(child <- branch.children)
                    box += generateBoxes(child);
                
                branch.properties += "box" -> box;
                box;
            }
            generateBoxes(tree);
            tree;
            //seal here:P
        };

        def renderTree:Object=>Unit = (data:Object)=>data.asInstanceOf[Branch].doAll(_.render);
        
        var tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(0,-worldSize+2.5f,-worldSize/2+30);
        tree.compile();
        trees += tree
        
        tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(17,-worldSize+2.5f,-worldSize/2+30);
        tree.compile();
        trees += tree

        tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(-17,-worldSize+2.5f,-worldSize/2+30);
        tree.compile();
        trees += tree
        /*
        tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(-34,-worldSize+2.5f,-worldSize/2+30);
        tree.compile();
        trees += tree

        tree = new GeneratorModel(giveMeTree, renderTree);
        tree.setPosition(34,-worldSize+2.5f,-worldSize/2+30);
        tree.compile();
        trees += tree
        */
        models = models ++ List(pig, catapult, terrain) ++ trees;
        generatedModels = models.filter(_.isInstanceOf[GeneratorModel]).map(_.asInstanceOf[GeneratorModel]);
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
        glClearColor(0.3f,0.6f,0.8f,1f);

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
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity;
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;
    }
  
    var frameIndepRatio = (48000000f);
    var treeView = false;
    var pause = false

    def moveObj = if(treeView) trees(0) else { if(pigcatapultLink.isLinked) catapult else pig };
    
    /**
    * Renders current frame
    */
    def renderFrame {
        if(!pause) {
            // move pig or catapult
            moveObj.vector.z -= 0.05f*moveObj.vector.z*renderTime;
            moveObj.vector.clamp(0,0,8);

            val moveVector = new Vec3(
                math.sin(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z,
                moveObj.vector.y,
                math.cos(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z
            )

            moveObj.pos.clamp(worldSize-2.5f);
            val mY = pig.pos.y;
            moveObj.pos += moveVector*renderTime;
            moveObj.pos.clamp(worldSize-2.5f);
            if(pig.pos.y==mY && settings.get[Boolean]("air")) {
                settings += "air"->false;println("pig is on ground");
                val trailcount = 3///
                if(trails.length >= trailcount) 
                    trails = trails.drop(1);
            }
            
            def unrot(f:Float, lim:Float=360f):Float = {
                var out = f;
                if(out>0) while(out > +lim) out -= lim; else
                if(out<0) while(out < -lim) out += lim;
                out;
            }
            
            pig.rot.x = unrot(pig.rot.x);
            
            pig.vector2 -= pig.vector2*renderTime*0.02f;
            if(settings.get[Boolean]("air")) {
                trails.last += moveObj.pos; 
            } else if(math.abs(pig.rot.x) > 5f) {//pri malo fps bo naredil več prevalov kot sicer, lol
                pig.vector2 -= pig.vector2*renderTime*0.01f;//trenje, lol
            } else {
                pig.vector2 = new Vec3;
            }
            
            pig.vector += gravity*renderTime            
            pig.rot += pig.vector2*renderTime

            pigcatapultLink.applyLink;
            campigLink.applyLink;
            
            // collision detection
            for(tree <- trees) if(tree.visible) {
                var done = false;
                var collision = false;
                var branch = tree.data.asInstanceOf[Branch];
                branch.doWhile((branch)=>{!done}, 
                    (branch)=>if(branch.visible) {
                        val box = branch.properties.get[BoundingBox]("box");
                        if(branch.depth==1 && !box.pointCollide(pig.pos, tree.pos)) {
                            done = true;// no collision
                        } else if(branch.depth>1 && box.pointCollide(pig.pos, tree.pos)) {
                            //if (branch.depth > 2) branch.visible = false;
                            collision = true;
                            
                            def dropBranch(b:Branch):Unit = {
                                b.detach;
                                val rootV = b.rootVec.clone;
                                b.doAll((_.rootVec -= rootV));
                                
                                //if(rand.nextFloat > 0.3) {
                                    branch.children.foreach((bc)=>{
                                        //if(rand.nextFloat > 0.3) 
                                            dropBranch(bc);
                                    })
                                //}
                                
                                var drop = new GeneratorModel(()=>{b}, (data:Object)=>data.asInstanceOf[Branch].doAll(_.render));
                                drop.pos = tree.pos + rootV;
                                drop.vector = new Vec3(
                                    math.sin(pig.rot.y/(180f/math.Pi)).toFloat*pig.vector.z/2 + rand.nextFloat/2 - rand.nextFloat/2,
                                    pig.vector.y/(5 + rand.nextFloat/3 - rand.nextFloat/3), 
                                    math.cos(pig.rot.y/(180f/math.Pi)).toFloat*pig.vector.z/2 + rand.nextFloat/2 - rand.nextFloat/2
                                )
                                drop.compile();
                                dropBranches += drop
                            }
                            
                            pig.vector.y /= 2;
                            
                            if(rand.nextFloat > 0.4) {
                                branch.children.foreach((bc)=>{if(rand.nextFloat > 0.2) dropBranch(bc)});
                            } else {
                                dropBranch(branch);
                            }
                            
                            println("collision");
                        }
                    }
                );
                if(collision) tree.compile;
            }
            // drop branches
            for(branch <- dropBranches) {
                branch.vector += gravity/(5 + rand.nextFloat/3 - rand.nextFloat/3)*renderTime;
                //branch.vector -= branch.vector*renderTime*0.05f;
                branch.pos += branch.vector*renderTime;
                if(branch.pos.y < -worldSize-50) {
                    dropBranches -= branch;
                    println("removed dropped branch");
                }
            }
        }
                
        //look at this pig... look!
        cam.lookAt(moveObj)
        cam.vector -= cam.vector*renderTime*0.05f;
        cam.pos += cam.vector*renderTime;
        cam.render

        for(model <- models ++ dropBranches ++ trails) if(model.visible) {
            glPushMatrix;
            model.doTransforms
            model.render
            glPopMatrix;
        }
    }
    
    def processInput {
        import Keyboard._ // static imports.. fuckyeah
        
        if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
            isRunning = false;
            return;
        }
                
        if(isKeyDown(KEY_T) && !timeLock.isLocked) {
            treeView = !treeView
            timeLock.lockIt(500);
        }
        if(isKeyDown(KEY_1)) { settings += "fatlines" -> false; trees.foreach(_.compile()) } else 
        if(isKeyDown(KEY_2)) { settings += "fatlines" -> true; trees.foreach(_.compile()) } else
        if(isKeyDown(KEY_3)) { trees.foreach(_.regenerate()) } else
        if(isKeyDown(KEY_5)) { 
            settings += "graphics" -> (settings.get[Float]("graphics")+1f);
            models.foreach(_.compile()) 
        } else
        if(isKeyDown(KEY_6) && settings.get[Float]("graphics") > 1) { 
            settings += "graphics" -> (settings.get[Float]("graphics")-1f);
            models.foreach(_.compile()) 
        } else
        if(isKeyDown(KEY_8) && !timeLock.isLocked) { pig.regenerate(); timeLock.lockIt(200); } else
        if(isKeyDown(KEY_9) && !timeLock.isLocked) { pig.compile(); timeLock.lockIt(200); }
        
        val keymove = 1.5f*renderTime;
        
        if(campigLink.isLinked) {
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

        if(isKeyDown(KEY_Z) && !timeLock.isLocked){
            terrain.visible = !terrain.visible
                
            timeLock.lockIt(100);            
        }

        if(isKeyDown(KEY_LEFT))  moveObj.rot.y+=keymove*3f;
        if(isKeyDown(KEY_RIGHT)) moveObj.rot.y-=keymove*3f;
        if(isKeyDown(KEY_UP))    moveObj.vector.z+=keymove/5f;
        if(isKeyDown(KEY_DOWN))  moveObj.vector.z-=keymove/5f;
        
        if(isKeyDown(KEY_SPACE) && (!settings.get[Boolean]("air"))) {
            if(pigcatapultLink.isLinked) {
                pigcatapultLink.breakLink;
                println("pig-catapult Link Broken")
                campigLink.breakLink
                println("cam-pig Link Broken")
                pig.vector.y=4.5f;
                pig.vector.z=7f;
                cam.vector = pig.vector / 3;
                pig.vector2 = new Vec3(0.5f+rand.nextFloat/3,0,0) * 50f;
            } else {
                pig.vector.y=3f;
                pig.vector.z=4f;
            }
            
            
            settings += "air" -> true;println("pig is in air");
            trails += new TrailModel(List(pig.pos))
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
            println("Pig-rot: "+pig.rot.toString);
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



