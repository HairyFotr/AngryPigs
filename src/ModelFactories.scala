package AngryPigs
import Global._
import org.lwjgl.opengl.GL11._

object TerrainFactory {
    // terrain
    val detail=30;
    val height=0.3f;
    
    private def genTerrain:()=>Object = ()=>{
        def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
        val p = (for(i <- 0 to detail; j <- 0 to detail) yield getTerrainPoint(i,j)).toArray;
        p;
    }
    private def drawTerrain = (data:Object)=>{
        val points = data.asInstanceOf[Array[Vec3]];
        glBegin(GL_QUADS);
        // Draw in clockwise - (00,10,11,01); must skip last point of line
        val width = math.sqrt(points.length).toInt;
        for(i <- 0 until points.length-width-1; if((i+1)%width != 0))
            List(points(i), points(i+1), points(i+width+1), points(i+width)).foreach(
                (p:Vec3) => {
                    //glColor3f(0.2f, 0.7f+p.y/2, 0.2f);
                    glColor3f(0.2+p.y/4, 0.65f+p.y/1.5, 0.2+p.y/4);
                    glNormal3f(p.y, p.y, p.y);
                    glVertex3f(p.x, p.y, p.z);
                }
            )
        glEnd;//*/
        ();
    }
    
    def apply() = new GeneratorModel(genTerrain, drawTerrain);
}
object PigFactory {
    private def genPig:()=>Object = ()=>{
        var pigData = new SettingMap[String];
        pigData += "Moustache.has" -> (rand.nextFloat > 0.2)
        pigData += "Moustache.which" -> (rand.nextInt(2))
        pigData += "Glasses.has" -> (rand.nextFloat > 0.2)
        pigData += "Glasses.which" -> (rand.nextInt(3))
    }    
    private def drawPig(data:Object):Unit = {
        var pigData = data.asInstanceOf[SettingMap[String]];
        val graphics = settings.get[Int]("graphics");
        //body
        glColor3f(0.3f,0.8f,0.3f);
        glPushMatrix;
        {
            glScalef(0.95f,1,1.05f);
            gluQuadrics.sphere.draw(2,graphics*16,graphics*16);
        }
        glPopMatrix
        //ears
        glColor3f(0.4f,0.9f,0.4f);
        glPushMatrix;
        {
            val x = 0.9f;
            glRotatef(180,0,1,0)
            glTranslatef(x,1.7f,-0.7f);
            gluQuadrics.disk.draw(0,0.35f, graphics*8,1);
            glTranslatef(-2*x,0,0);
            gluQuadrics.disk.draw(0,0.35f, graphics*8,1);
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
            gluQuadrics.cylinder.draw(size,size, 1, graphics*12,1);
            glTranslatef(0,0,1);
            gluQuadrics.disk.draw(0,size, graphics*12,1);
        }
        //moustache
        if(pigData.get[Boolean]("Moustache.has")) {
            glScalef(2,1,1);
            glColor3f(0.7f,0.2f,0f);
            pigData.get[Int]("Moustache.which") match {
                case 0 =>                
                    glTranslatef(0,-0.7f,-0.2f)
                    gluQuadrics.disk.draw(0,0.5f, graphics*9,1);
                case 1 =>
                    glTranslatef(0,-0.8f,-0.3f)
                    gluQuadrics.partialdisk.draw(0,0.5f, graphics*9,1, 270, 180);
                case _ =>
                    glTranslatef(0,-0.8f,-0.3f)
                    gluQuadrics.partialdisk.draw(0,0.5f, graphics*9,1, 270, 180);
            }
        }
        glPopMatrix
        //eyes
        glPushMatrix;
        {
            val x = 1.2f;
            def drawEye(leftEye:Boolean) = {
                glPushMatrix;
                glColor3f(0.8f,0.8f,0.8f);
                gluQuadrics.sphere.draw(0.5f,graphics*8,graphics*8);
                val z = 0.35f;
                glTranslatef(0,0,z);
                glColor3f(0.1f,0.1f,0.1f);
                gluQuadrics.sphere.draw(0.25f,graphics*8,graphics*8);
                glTranslatef(0,0,0.1f);
                if(pigData.get[Boolean]("Glasses.has")) {
                    pigData.get[Int]("Glasses.which") match {
                        case 0 => // ray-bans
                            glTranslatef(0,0.2f,0.16f);
                            gluQuadrics.disk.draw(0.60f,0.70f, graphics*10,1);
                            glColor3f(0.20f,0.15f,0.15f);
                            gluQuadrics.disk.draw(0,0.60f, graphics*10,1);
                        case 1 => // monocle
                            if(leftEye){
                                glColor3f(0.95f,0.8f,0.1f);
                                gluQuadrics.disk.draw(0.62f,0.70f, graphics*10,1);
                            }
                        case _ => // harry-potter
                            gluQuadrics.disk.draw(0.67f,0.77f, graphics*10,1);
                    }
                }
                glPopMatrix
            }
            glTranslatef(x,0.6f,1.2f);
            drawEye(true);
            glTranslatef(-2*x,0,0);
            drawEye(false);
        }
        glPopMatrix
    }
    
    def apply() = new GeneratorModel(genPig, drawPig);
}

object CatapultFactory {
    val scale = new Vec3(4f,1f,6.5f)
    def apply() = {
        var catapult = new DisplayModel(()=>{
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
                gluQuadrics.cylinder.draw(1f,1f, scale.x*2+2, settings.get[Int]("graphics")*9,1);
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
        val bbox = new BoundingBox(new Vec3);
        bbox.min -= scale
        bbox.max += scale
        catapult.properties += "box" -> bbox;
        catapult;
    }
}

object TreeFactory {
    private def giveMeTree:()=>Object = ()=>{
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
                res.properties += (if(rand.nextFloat < 0.085*res.depth) "hasLeaf" -> true else "hasLeaf" -> false);
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
                data = genTree/("give-me-tree", 
                0f+rand.nextFloat()/10-rand.nextFloat()/10, 
                2f+rand.nextFloat()/2-rand.nextFloat()/3, 
                0f+rand.nextFloat()/10-rand.nextFloat()/10, 
                5f+rand.nextFloat()-rand.nextFloat()/2);
        } catch {
            case _ => {
                //e.printStackTrace;
                println("give-me-tree threw exception");                    
                data = null;
            }
        }
        
        val tree = traverse(asArray(data));
        tree.properties += "treekind" -> 0;//rand.nextInt(3);
        tree.properties += "fatness" -> (0.25f+rand.nextFloat()/20f-rand.nextFloat()/20f);
        
        def generateBoxes(branch:Branch):BoundingBox = {
            var box = new BoundingBox(List(branch.rootVec, branch.destVec));
            
            for(child <- branch.children)
                box += generateBoxes(child);
            
            branch.properties += "box" -> box;
            if(branch.children.length==0) {
                branch.properties += "fatness" -> (0.18f-rand.nextFloat()/30f);
            } else {
                branch.properties += "fatness" -> (0.2f-rand.nextFloat()/30f);
            }
            box;
        }
        generateBoxes(tree);
        
        tree;
    };
    
    private def renderTree:Object=>Unit = (data:Object)=>data.asInstanceOf[Branch].doAll(_.render);
    private def treeId:(DisplayModel,SettingMap[String])=>Int = (dmodel,properties)=>{
        val model = dmodel.asInstanceOf[GeneratorModel];
        var mid = 0;///to properties on fly
        // takes into account branch count and graphic detail
        model.data.asInstanceOf[Branch].doAll((branch)=>{ mid += 1 })
        mid += mid * properties.get[Int]("graphics");
        mid += mid + 101*properties.get[Int]("maxdepth");
        mid;
    };
    
    def apply() = {
        import Global._
        var tree = new GeneratorModel(giveMeTree, renderTree, treeId);
        def random(span:Float):Float = {
            (17+rand.nextFloat()*3-rand.nextFloat()*3)*rand.nextFloat()*span;
        }
        tree.setPosition(
            random(10) - random(10),
            -Settings.worldSize,
            -Settings.worldSize/2+30 + random(10) - random(7)
        );
        tree
    }
}
