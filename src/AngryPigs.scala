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
    
    var terrain:QuadPatch=null;
    // @would it pay-off to make model generation lazy and generate them on the fly?
    // @infinite terrain patches and stuff
    def makeModels {
        // Terrain
        val detail=50;
        val height=0.3f;
        
        def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
        val p = (for(i <- 0 until detail; j <- 0 until detail) yield getTerrainPoint(i,j)).toArray;
        
        terrain = new QuadPatch(p, detail);
    }

    /**
	 * Initial setup of projection of the scene onto screen, lights etc.
	 */
    def setupView {
        GL11.glEnable(GL11.GL_DEPTH_TEST); // enable depth buffer (off by default)
        //GL11.glEnable(GL11.GL_CULL_FACE);  // enable culling of back sides of polygons
      
        GL11.glViewport(0,0, winWidth,winHeight); // mapping from normalized to window coordinates
	   
        cam.setPerspective(45, 4/3f, 1f, 50f);
        cam.setPosition(0,0,2f);
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
        cam.render;
        terrain.render;
        
        // coordinate system
        /*GL11.glBegin(GL11.GL_LINES);
            GL11.glColor3f(1,0,0);
            GL11.glVertex3f(0,0,0);
            GL11.glVertex3f(100,0,0);
            
            GL11.glColor3f(0,1,0);
            GL11.glVertex3f(0,0,0);
            GL11.glVertex3f(0,100,0);
            
            GL11.glColor3f(0,0,1);
            GL11.glVertex3f(0,0,0);
            GL11.glVertex3f(0,0,100);
        GL11.glEnd;*/
        
        // sky-box
        
        GL11.glPushMatrix;
        GL11.glTranslatef(0,-1,0);
        GL11.glScalef(5,5,5);
        GL11.glBegin(GL11.GL_QUADS);
		    GL11.glColor3f(0.0f,1.0f,0.0f);						// Set The Color To Green
		    GL11.glVertex3f( 1.0f, 1.0f,-1.0f);					// Top Right Of The Quad (Top)
		    GL11.glVertex3f(-1.0f, 1.0f,-1.0f);					// Top Left Of The Quad (Top)
		    GL11.glVertex3f(-1.0f, 1.0f, 1.0f);					// Bottom Left Of The Quad (Top)
		    GL11.glVertex3f( 1.0f, 1.0f, 1.0f);					// Bottom Right Of The Quad (Top)
		    
		    GL11.glColor3f(1.0f,0.5f,0.0f);						// Set The Color To Orange
		    GL11.glVertex3f( 1.0f,-1.0f, 1.0f);					// Top Right Of The Quad (Bottom)
		    GL11.glVertex3f(-1.0f,-1.0f, 1.0f);					// Top Left Of The Quad (Bottom)
		    GL11.glVertex3f(-1.0f,-1.0f,-1.0f);					// Bottom Left Of The Quad (Bottom)
		    GL11.glVertex3f( 1.0f,-1.0f,-1.0f);					// Bottom Right Of The Quad (Bottom)
		    
		    GL11.glColor3f(1.0f,0.0f,0.0f);						// Set The Color To Red
		    GL11.glVertex3f( 1.0f, 1.0f, 1.0f);					// Top Right Of The Quad (Front)
		    GL11.glVertex3f(-1.0f, 1.0f, 1.0f);					// Top Left Of The Quad (Front)
		    GL11.glVertex3f(-1.0f,-1.0f, 1.0f);					// Bottom Left Of The Quad (Front)
		    GL11.glVertex3f( 1.0f,-1.0f, 1.0f);					// Bottom Right Of The Quad (Front)
		    
		    GL11.glColor3f(1.0f,1.0f,0.0f);						// Set The Color To Yellow
		    GL11.glVertex3f( 1.0f,-1.0f,-1.0f);					// Top Right Of The Quad (Back)
		    GL11.glVertex3f(-1.0f,-1.0f,-1.0f);					// Top Left Of The Quad (Back)
		    GL11.glVertex3f(-1.0f, 1.0f,-1.0f);					// Bottom Left Of The Quad (Back)
		    GL11.glVertex3f( 1.0f, 1.0f,-1.0f);					// Bottom Right Of The Quad (Back)
		    
		    GL11.glColor3f(0.0f,0.0f,1.0f);						// Set The Color To Blue
		    GL11.glVertex3f(-1.0f, 1.0f, 1.0f);					// Top Right Of The Quad (Left)
		    GL11.glVertex3f(-1.0f, 1.0f,-1.0f);					// Top Left Of The Quad (Left)
		    GL11.glVertex3f(-1.0f,-1.0f,-1.0f);					// Bottom Left Of The Quad (Left)
		    GL11.glVertex3f(-1.0f,-1.0f, 1.0f);					// Bottom Right Of The Quad (Left)
		    
		    GL11.glColor3f(1.0f,0.0f,1.0f);						// Set The Color To Violet
		    GL11.glVertex3f( 1.0f, 1.0f,-1.0f);					// Top Right Of The Quad (Right)
		    GL11.glVertex3f( 1.0f, 1.0f, 1.0f);					// Top Left Of The Quad (Right)
		    GL11.glVertex3f( 1.0f,-1.0f, 1.0f);					// Bottom Left Of The Quad (Right)
		    GL11.glVertex3f( 1.0f,-1.0f,-1.0f);					// Bottom Right Of The Quad (Right)
        GL11.glEnd;
        GL11.glPopMatrix;
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
        
        if(Keyboard.isKeyDown(Keyboard.KEY_LEFT))  terrain.pos.x+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) terrain.pos.x-=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_UP))    terrain.pos.z+=0.7f;
        if(Keyboard.isKeyDown(Keyboard.KEY_DOWN))  terrain.pos.z-=0.7f;

        if(Keyboard.isKeyDown(Keyboard.KEY_P)) {
            println(cam.toString);
            println(terrain.toString);
        }
    }    
}






