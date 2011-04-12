import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import scala.util._;

// TODO:
// search for @task

object AngryPigs {

    var isRunning = false; // is main loop running
    val (winWidth, winHeight)=(800,600); // window size
    val cam = new Camera;
    val rand = new Random;
    
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
        mainLoop;
        
        // cleanup
        Display.destroy;
    }

    def initDisplay {
        var bestMode:DisplayMode = null;
        val modes:Array[DisplayMode] = Display.getAvailableDisplayModes;
        // Get best mode
        for(mode <- modes)
            if((mode.getWidth == winWidth && mode.getHeight == winHeight && mode.getFrequency <= 85)
                &&(bestMode == null
                   ||(mode.getBitsPerPixel >= bestMode.getBitsPerPixel
                      && mode.getFrequency > bestMode.getFrequency)))
                bestMode = mode;
        
        Display.setDisplayMode(bestMode);
        // FSAA
        //Display.create(new PixelFormat(8, 8, 8, 4));
        // No FSAA
        Display.create;
        Display.setTitle("Angry Pigs");
    }
    /**
     * Main loop: renders and processes input events
     */
    def mainLoop { 
        //loadModels; // load models
        makeModels; // make generative models
        setupView;  // setup camera and lights
    
        // @that is one ugly FPS counter :)
        def now = System.nanoTime();
        var secondTimer = now;
        var frameCounter = 0;
        val E10 = 10000000000L;
        while(isRunning) {
            frameCounter += 1;
            //val start = now;
            resetView;      // clear view and reset transformations
            renderFrame;    // draw stuff
            //gl error
            val errCode = GL11.glGetError;
            if (errCode != GL11.GL_NO_ERROR) 
                println(opengl.Util.translateGLErrorString(errCode));
            // @menda se da sproti/bolš gledat input
            processInput;   // process input events 
            Display.update; // update window contents and process input messages
            //val timeSpent = now-start;
            if(now-secondTimer > E10) {
                secondTimer = now;
                // print fps
                println("FPS: "+frameCounter/10);
                frameCounter = 0;
            }
        }
    }
    
    //models
    var terrain:QuadPatch=null;
    var skybox:DisplayModel=null;
    var coordsys:DisplayModel=null;
    var pig:DisplayModel=null;
    
    // @would it pay-off to make model generation lazy and generate them on the fly?
    // @infinite terrain patches and stuff
    def makeModels {
        val scale = 50;//size of world
        // terrain
        val detail=10;
        val height=0.1f;
        
        def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
        val p = (for(i <- 0 to detail; j <- 0 to detail) yield getTerrainPoint(i,j)).toArray;        
        terrain = new QuadPatch(p, detail+1);
        terrain.setPosition(-scale,-scale,-scale);
        terrain.setScale(scale*2, 5, scale*2);
        
                // coordinate system
        coordsys = new DisplayModel(Unit=>{
            GL11.glBegin(GL11.GL_LINES);
                GL11.glColor3f(1,0,0);
                GL11.glVertex3f(0,0,0);
                GL11.glVertex3f(1,0,0);
                
                GL11.glColor3f(0,1,0);
                GL11.glVertex3f(0,0,0);
                GL11.glVertex3f(0,1,0);
                
                GL11.glColor3f(0,0,1);
                GL11.glVertex3f(0,0,0);
                GL11.glVertex3f(0,0,1);
            GL11.glEnd;//*/
        });
        coordsys.setScale(scale,scale,scale);

        // sky-box
        skybox = new DisplayModel(Unit=>{
            GL11.glBegin(GL11.GL_QUADS);
                // top
                GL11.glColor3f(0f,1f,0f); // green
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                // bottom 
                /*GL11.glColor3f(1f,0.5f,0f);    // orange
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f( 1f,-1f,-1f);*/
                // Front
                GL11.glColor3f(1f,0f,0f); // red 
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f, 1f); 
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                // back
                GL11.glColor3f(1f,1f,0f); // yellow
                GL11.glVertex3f( 1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f,-1f);
                // left
                GL11.glColor3f(0f,0f,1f); // blue
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                // right
                GL11.glColor3f(1f,0f,1f); // violet
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f,-1f);
            GL11.glEnd;
        });
        skybox.setPosition(0,0,0);
        skybox.setScale(scale,scale,scale);//*/

        // pig
        pig = new DisplayModel(Unit=>{
            GL11.glColor3f(0.2f,0.7f,0.2f);
            val p = new Sphere();
            p.draw(2,10,10);
        });
        pig.setPosition(0,-scale+2,-15);
        //pig.setScale(scale/,scale,scale);//*/
    }

    /**
     * Initial setup of projection of the scene onto screen, lights etc.
     */
    def setupView {
        GL11.glEnable(GL11.GL_DEPTH_TEST); // enable depth buffer (off by default)
        //GL11.glEnable(GL11.GL_CULL_FACE);  // enable culling of back sides of polygons
      
        GL11.glViewport(0,0, winWidth,winHeight); // mapping from normalized to window coordinates
       
        //GL11.glHint(GL11.GL_PERSPECTIVE_CORRECTION_HINT, GL11.GL_NICEST);
        cam.setPerspective(45, winWidth/winHeight.toFloat, 1f, 500f);
        cam.setPosition(0,45,-50);
        cam.setRotation(0,180,0);
    }
  
    /**
    * Resets the view of current frame
    */
    def resetView {
        // clear color and depth buffer
        GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity;
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity;
    }
  
    /**
    * Renders current frame
    */
    def renderFrame {
        List(
            cam,
            terrain,
            skybox,
            coordsys,
            pig
        ).map(_.render);
    }
    
    def processInput {
        if(Display.isCloseRequested || Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) isRunning = false;
        
        if(Keyboard.isKeyDown(Keyboard.KEY_Q)) cam.rot.x+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_E)) cam.rot.x-=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_A)) cam.rot.y+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_D)) cam.rot.y-=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_Y)) cam.rot.z+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_C)) cam.rot.z-=0.7f;

        if(Keyboard.isKeyDown(Keyboard.KEY_W)) cam.pos.x+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_R)) cam.pos.x-=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_S)) cam.pos.y+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_F)) cam.pos.y-=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_X)) cam.pos.z+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_V)) cam.pos.z-=0.7f;
        
        if(Keyboard.isKeyDown(Keyboard.KEY_LEFT))  pig.pos.x+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) pig.pos.x-=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_UP))    pig.pos.z+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_DOWN))  pig.pos.z-=0.7f;

        if(Keyboard.isKeyDown(Keyboard.KEY_P)) {
            println(cam.toString);
            println(pig.toString);
        }
    }    
}






