package base

class Event(name:String,args:List[Int]) { 
  def expand = this
  override def toString = {
    if(args.isEmpty) name
    else name+args.mkString("(",",",")")
  }
}