package state_machine

import base.Event

class StateMachine[NameType] {
  var delta : Map[(Int,Event),Int] = Map()
  var accepts : Set[Int] = Set()
  
  var names : Map[Int,NameType] = Map()
  
  def name(s:Int,f:NameType){
    if(names.get(s).isDefined) throw new RuntimeException("Already named "+s)
    names += (s -> f)
  }
  
  def addTransition(from:Int,e:Event,to:Int){
    if(delta.get((from,e)).isDefined) throw new RuntimeException("Overwriting a transition")
    if(from < 0 || to < 0) throw new RuntimeException("Negative state")
    delta += ((from,e) -> to)
  }
  def next(from:Int,e:Event) : Int = {
    if(delta.get((from,e)).isDefined) delta((from,e))
    else -1
  }
  def makeAccepting(state:Int){ accepts+=state}
  def isAccepting(state:Int) : Boolean = { accepts.contains(state)}
  
  def isComplete : Boolean = {
    val events = delta.map{ case ((_,e),_) => e}.toSet
    val states = delta.flatMap{ case((s1,e),s2) => Set(s1,s2)}.toSet
    var ret = (1 to states.size).forall{s => states.contains(s)}
    ret &= states.forall{s => events.forall{e => delta.get((s,e)).isDefined}}
    ret &= states.forall{s => names.get(s).isDefined}
    ret
  }
  
  def events = delta.map{ case ((_,e),_) => e}.toSet.toList
  
  override def toString : String = {
    val states = delta.flatMap{ case((s1,e),s2) => Set(s1,s2)}.toSet.toList.sorted
    val events = delta.map{ case ((_,e),_) => e}.toSet.toList
    var r = ""
     
    states.foreach{s => 
    	val status = if(isAccepting(s)) "accept" else ""
    	r+=status+" s"+s+"\n"
    	events.foreach{e => r+="\t"+e+"\t->\t"+delta((s,e))+"\n"}
    }  
      
    r+="\n\n"
    states.foreach{s => r+=s+"\t\t"+names(s)+"\n"}
    r
  }
}