package temporal_logic

import Translation._
import Formula._

object Testing extends App {
  
  def once(e:Event) : Formula = AlwaysImplies(e,NextUntil(Not(e),False))
  
  val HN = And(Until(Not("next"),"hasnext"),AlwaysImplies("next",NextUntil(Not("next"),"hasnext")))
  val UM = Eventually(And("create",Eventually(And("iterator",Eventually(And("update",Eventually("use")))))))
  val CN = And(
		  Until(Not("return1"),"call1"),
		  Until(Not("return2"),"call2"),
		  AlwaysImplies("callm1",(And(
				  Until(Not("call1"),"return1"),
				  Until(LeftAnd(Not("return2"),Not("call2")),"return2"),
				      "return1"
				  )
				  ))
		  )
      
	//val f = Eventually(And("a",Eventually(And("b",Eventually("c")))))	  
	
 val (a,b,c) = ("a","b","c")
 /*
 val four = True
 val three = Until(Not(c),Or(a,b),And(c,four))
 val two   = Until(Not(b),Or(a,c),And(b,three))
 val one   = Until(Not(a),Or(b,c),And(a,two))
 */
 
 val two = Until(Not(b),Until(Not(b),a,False),False)
 val one = Until(Not(a),Until(Not(a),b,And(a,two)),False)
		  
 val fsm = toStates(one)
 println(fsm)
 
 

  
}
