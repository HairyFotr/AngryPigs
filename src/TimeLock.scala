package AngryPigs

class TimeLock {
    private var lock = false;
    def isLocked:Boolean = {
        if(lock) {
            if(milliTime-lockTime > lockDuration) {
                lock = false;
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }
    
    private def milliTime = System.nanoTime()/1000000L
    
    private var lockTime = milliTime;
    private var lockDuration = 0L;
    def lockIt(ms:Int) = {
        lockTime = milliTime;
        lockDuration = ms;
        lock = true
    }
}

