import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import java.nio._

//Scala-Java static hack: http://www.eishay.com/2009/05/scala-java-interoperability-statics.html
object ShowWindow {
    def main(args: Array[String]) {
        // Start our program
        (new Window()).execute();
    }
}

class Window {
    var isRunning = false;
    val (winX, winY)=(800,600);
    
    /**
     * Initializes display and enters main loop
     */
    def execute(){
        try {
          initDisplay();
        } catch {
            case e:LWJGLException => {
              System.err.println("Can't open display. "+e.getMessage());
              System.exit(0);
            }
        }

        isRunning = true;
        mainLoop();
        Display.destroy();
    }

    /**
     * Main loop: renders and processes input events
     */
    def mainLoop() { 
        // setup camera and lights
        setupView();

        while(isRunning) {
            // reset view
            resetView();

            // let subsystem paint
            renderFrame();

            // process input events
            processInput();

            // update window contents and process input messages
            Display.update();
        }
    }

    /**
     * Initial setup of projection of the scene onto screen, lights, etc.
     */
    def setupView() {}

    /**
     * Resets the view of current frame
     */
    def resetView() {}

    /**
     * Renders current frame
     */
    def renderFrame() {}

    /**
     * Processes Keyboard and Mouse input and spawns actions
     */
    def processInput() {
        if(Display.isCloseRequested() || Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) isRunning = false;
    }

    /**
     * Finds best display mode and sets it
     * 
     * @throws LWJGLException
     */
    //@throws(classOf[LWJGLException])
    def initDisplay() {
        var bestMode:DisplayMode = null;
        val mode:Array[DisplayMode] = Display.getAvailableDisplayModes;
        for(i <- 0 until mode.length)
            if((mode(i).getWidth == winX && mode(i).getHeight == winY && mode(i).getFrequency <= 85) 
                &&(bestMode == null
                   ||(mode(i).getBitsPerPixel >= bestMode.getBitsPerPixel 
                      && mode(i).getFrequency > bestMode.getFrequency)))
                bestMode = mode(i);

        Display.setDisplayMode(bestMode);
        // FSAA
        //Display.create(new PixelFormat(8, 8, 8, 4));
        // No FSAA
        Display.create;
        Display.setTitle(this.getClass.getName);
    } 
}
