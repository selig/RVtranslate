package temporal_logic

import state_machine.StateMachine
import Formula._

object Translation{
  
  def getEvents(f:Formula) : Set[Event] = {
    f match {
      case e : Event => Set(e)
      case NegatedEvent(e) => Set(e)
      case Not(f) => getEvents(f)
      case And(fs) => fs.flatMap(getEvents).toSet
      case LeftAnd(fs) => fs.flatMap(getEvents).toSet
      case Or(fs) => fs.flatMap(getEvents).toSet
      case Until(l1,l2,r) => Set(l1,l2,r).flatMap(getEvents)
      case NextUntil(l1,l2,r) => Set(l1,l2,r).flatMap(getEvents)
      case True => Set()
    }
  }
  
  def toStates(in:Formula) : StateMachine[Formula] = {
    var f : Formula = in.expand
    //println("Expanded: "+f)
    f = f.simplify
    //println("Simplified: "+f)
    //println
    
    val fsm = new StateMachine[Formula]
    
    val events = getEvents(f)
    
    var states : Map[Formula,Int] = Map()
    def getState(f:Formula,add:Boolean=true) : (Int,Boolean) = {
      val s = states.get(f)
      if(s.isDefined) (s.get,false)
      else{
        if(!add) (0,false)
        else{
	        val n = states.size+1
	        states += (f -> n)
	        (n,true)
        }
      }
    }
    def rewriteSubTerms(f:Formula,top:Boolean=true) : Formula = {
      if(!top){
    	  val (n,_) = getState(f,false)
    	  if(n!=null) return Event("s"+n)
      }
      f match {
        case And(fs) => And(fs.map(rewriteSubTerms(_,false)))
        case Or(fs) => Or(fs.map(rewriteSubTerms(_,false)))
        case _ => f
      }
      
    }
    
    val start = getState(f)._1
    var next = Set(f)
    var i = 0
    while(!next.isEmpty){
      var nextnext = Set[Formula]()
      for(s<-next){        
        val sname = getState(s)._1
        if(accepts(s)) fsm.makeAccepting(sname) 
        for(e<-events){
          val nf = s.progress(e).simplify
          val (ns,isnew) = getState(nf)
          if(isnew) nextnext+=nf
          fsm.addTransition(sname,e,ns)
          //println("\t"+e+"\t->\t"+ns)
        }
      }
      next = nextnext
      if(i==100000) next = Set() // to quit the cycle early
      i+=1
    }
    //println
    //println
    //println("State formulas:")
    //states.values.toList.sorted.foreach{ s => fsm.name(s,rewriteSubTerms(states.filter(_._2==s).head._1))}
    states.foreach{ case (f,s) => fsm.name(s,f)}
    
    if(!fsm.isComplete) throw new RuntimeException("Did not create a complete fsm")
    
    fsm
    
  }
  
  // WE ASSUME that states start with 1 as initial state
  def toFormula(fsm:StateMachine[Formula]) : Formula = toFormula(fsm,1)
  
  def toFormula(fsm:StateMachine[Formula],s:Int) : Formula = {
    
   val events = fsm.events
   
   // We
    
   null     
  }
  
}