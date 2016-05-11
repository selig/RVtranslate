package temporal_logic

trait Formula{
  def expand : ActualFormula
}
sealed trait ActualFormula extends Formula
trait LeftFormula extends ActualFormula{
  def expand = this
}

// Formulas
case class Event(name:String,args:List[Int]) extends base.Event(name,args) with ActualFormula { 
  def expand = this
  override def toString = {
    if(args.isEmpty) name
    else name+args.mkString("(",",",")")
  }
}
case class And(fs:List[Formula]) extends ActualFormula{
  def expand = And(fs.map(_.expand))
  override def equals(o:Any) : Boolean = {
    if(o.isInstanceOf[And]){
      o.asInstanceOf[And].fs.equals(fs)
    }
    else false
  }
  override def toString = "And"+fs.mkString("(",",",")")
}
case class Or(fs:List[Formula]) extends ActualFormula{
  def expand = Or(fs.map(_.expand))
  override def equals(o:Any) : Boolean = {
    if(o.isInstanceOf[Or]){
      o.asInstanceOf[Or].fs.equals(fs)
    }
    else false
  }  
  override def toString = "Or"+fs.mkString("(",",",")")
}
case class Not(f1:Formula) extends ActualFormula{
  def expand = Not(f1.expand)
  override def toString = {
    if(f1.isInstanceOf[Event]) "-"+f1
    else "Not("+f1+")"
  }
}
case class Until(l1:LeftFormula,l2:Formula,r:Formula) extends ActualFormula{
  def expand = Until(l1,l2.expand,r.expand)
  /*override def toString = {
    if((l2 equals Not(True)) || (l2 equals False)) "("+l1+" until "+r+")"  
    if(l1 equals True) "("+l2+" until "+r+")"
    "(Or("+l1+","+l2+") until "+r+")"
  }*/
}
case class NextUntil(l1:LeftFormula,l2:Formula,r:Formula) extends ActualFormula{
  def expand = NextUntil(l1,l2.expand,r.expand)
 /* override def toString = {
    if((l2 equals Not(True)) || (l2 equals False)) "("+l1+" nuntil "+r+")"  
    if(l1 equals True) "("+l2+" nuntil "+r+")"
    "(Or("+l1+","+l2+") nuntil "+r+")"    
  }*/
}
case object True extends ActualFormula with LeftFormula

// Left Formulas
case class NegatedEvent(e:Event) extends LeftFormula{
  override def toString = "-"+e
}
case class LeftAnd(fs:List[LeftFormula]) extends LeftFormula{
    override def equals(o:Any) : Boolean = {
    if(o.isInstanceOf[LeftAnd]){
      o.asInstanceOf[LeftAnd].fs.equals(fs)
    }
    else false
  }
}

object LeftAnd{
  def apply(fs:LeftFormula*) : LeftAnd = LeftAnd(fs.toList)
  implicit def toAnd(l:LeftAnd) : Formula = And(l.fs)
}
import LeftAnd._

// Derived Formulas
case object False extends Formula{
  override def expand : ActualFormula = Not(True)
}
case class Implies(f1:Formula,f2:Formula) extends Formula{
  override def expand : ActualFormula = Or(Not(f1.expand),f2.expand)
}
case class Eventually(f:Formula) extends Formula{
  override def expand : ActualFormula = Until(True,f.expand)
}
case class AlwaysImplies(e:Event,f:Formula) extends Formula{
  override def expand : ActualFormula = Until(Not(e),f.expand,Not(True))
}

// Alternative constructors
object Event {
  def apply(s:String,is:Int*) : Event = Event(s,is.toList)
  implicit def toEvent(s:String) : Event = Event(s,List())
}
import Event._
object Until{
  def apply(l1:LeftFormula,r:Formula) : Until = Until(l1,Not(True),r)
}
object NextUntil{
  def apply(l1:LeftFormula,r:Formula) : NextUntil = NextUntil(l1,Not(True),r)
}
object Not{
  def apply(e:Event) : NegatedEvent = NegatedEvent(e)
}
object And{
  def apply(fs:Formula*) : And = And(fs.toList)
}
object Or{
  def apply(fs:Formula*) : Or = Or(fs.toList)
}

// Simplifications
object Formula{
  
    val doDistribution=false
  
	implicit def toSimplify(f:Formula) = new {
	  def simplify() : Formula = {
	    var last = f
	    var next = simplifyStep(last)
	    while(!last.equals(next)){
	      last = next
	      next = simplifyStep(last)
	    }
	    return last
	  }
	  def progress(e:Event) : Formula = {
	    progressStep(f,e)
	  }	  
	}
	def simplifyStep(f:LeftFormula) : LeftFormula = {
	  f match{
	    case LeftAnd(Nil) => True
	    case LeftAnd(fs) => {
	      val filtered = fs.filter(_!=True).toSet.toList
	      LeftAnd(
	          filtered.filter(_.isInstanceOf[LeftAnd]).flatMap(_.asInstanceOf[LeftAnd].fs):::
	          filtered.filter(!_.isInstanceOf[LeftAnd])
	         )
	    }
	    case ne : NegatedEvent => ne
	    case True => True
	  }
	}
	
	def simplifyStep(f:Formula) : Formula = { 
	    //println("simp "+f)
	    f match{
	      // Reducing junctions
	      case And(Nil) => True	
	      case And(f::Nil) => simplifyStep(f)
	      case And(fs) => {
	        if(fs.contains(Not(True))) Not(True) else {
	          val notrue = fs.filter(_!=True).map(simplifyStep(_)).toSet.toList
	          //println("notrue:"+notrue)
	          ///Flatten
	          val ands = notrue.filter(_.isInstanceOf[And])
	          if(!ands.isEmpty){
	            val notands = notrue.filter(!_.isInstanceOf[And])
	            And(notands:::ands.flatMap(_.asInstanceOf[And].fs))
	          }
	          else{
		          // Distributivity
		          val ors = notrue.filter(_.isInstanceOf[Or])
		          if(!doDistribution || ors.isEmpty) And(notrue)
		          else{
		            //println("Doing distribution")
		            val nonors  = notrue.filter(!_.isInstanceOf[Or])
		            // we know nonors is and free so f is or(..),or(...),....
		            //flatten ors
		            Or(ors.flatMap(_.asInstanceOf[Or].fs).map(o => And(o::nonors)))
		          }
	          }
	        }
	      }
	      case Or(Nil) => Not(True)
	      case Or(f::Nil) => simplifyStep(f)
	      case Or(fs) => {
	        if(fs.contains(True)) True else{
	          // filter
	          val nofalse = fs.filter(_!=Not(True)).map(simplifyStep(_)).toSet.toList
	          //println("nofalse:"+nofalse)
	          // flatten
	          val ors = nofalse.filter(_.isInstanceOf[Or])
	          if(ors.isEmpty) Or(nofalse)
	          else{
	            val nonors = nofalse.filter(!_.isInstanceOf[Or])
	            Or(ors.flatMap(_.asInstanceOf[Or].fs):::nonors)
	          }
	        }
	      }
	      // push negations in
	      case Not(Not(f)) => simplifyStep(f)
	      case Not(Or(fs)) => And(fs.map(o => simplifyStep(Not(o))))
	      case Not(And(fs)) => Or(fs.map(o => simplifyStep(Not(o))))
	      // not pushable
	      case Not(f) => Not(simplifyStep(f))
	      // others
	      case e : Event => e
	      case True => True
	      case Until(l1,l2,r) => Until(simplifyStep(l1),simplifyStep(l2),simplifyStep(r))
	      case NextUntil(l1,l2,r) => NextUntil(simplifyStep(l1),simplifyStep(l2),simplifyStep(r))
	    }
	}
	
	def progressStep(f:Formula,e:Event) : Formula = {
	  val r = f match {
	    case True => True
	    case NegatedEvent(otherE) => Not(progressStep(otherE,e))
	    case otherE : Event => if(e.equals(otherE)) True else Not(True)
	    case Not(f) => Not(progressStep(f,e))
	    case And(fs) => And(fs.map(progressStep(_,e)))
	    case Or(fs) => Or(fs.map(progressStep(_,e)))
	    case Until(l1,l2,r) => Or(progressStep(r,e),And(f,Or(progressStep(l1,e),progressStep(l2,e))))
	    case NextUntil(l1,l2,r) => Until(l1,l2,r)
	  }
	  //println("progress "+f+"\ninto "+r+"\nwith "+e+"\n")
	  r
	}

	def accepts(f:Formula) : Boolean = {
	  f match {
	    case True => true
	    case e : Event => false
	    case NegatedEvent(e) => true
	    case Not(f) => !accepts(f)
	    case And(fs) => fs.forall(accepts)
	    case Or(fs) => fs.exists(accepts)
	    case Until(l1,l2,Not(True)) => true
	    case Until(l1,l2,r) => accepts(r)
	    case NextUntil(l1,l2,r) => false
	  }
	}
	
}




