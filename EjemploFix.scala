object DynamicElvis {

  class DynamicImpl( x: AnyRef ) extends Dynamic {
	  
    def _select_(name: String): DynamicElvis = {       	
      val mName = name.substring(0, name.length - 1  )
      val mVal = x.getClass.getMethod(mName).invoke( x )   	
      new DynamicElvis( if( mVal != null ) Some( mVal ) else None )
    }
    
    def _invoke_(name: String)(args: Any*) = {
      error("unsupported")
    }
  } 
  
  trait Elvis extends Dynamic{

    def _select_(name: String): DynamicElvis = {
      val mName = name.substring(0, name.length - 1  )
      val mVal = this.getClass.getMethod(mName).invoke( this )
      new DynamicElvis( if( mVal != null ) Some( mVal ) else None )
    }
    
    def _invoke_(name: String)(args: Any*) = {
      error("unsupported")
    }
  }
  
  class DynamicElvis( x: Option[AnyRef] ) extends Dynamic {
	  
    def _select_(name: String): DynamicElvis = {
      if( name.endsWith( "Q" ) ){
        val mName = name.substring( 0, name.length - 1  )
    	  new DynamicElvis(
    	    x.flatMap{ cVal =>
    	      val nVal = cVal.getClass.getMethod(mName).invoke( cVal )
    	      if( nVal != null ) Some( nVal ) else None } )
    	}
    	else{
    		error( name + "doesn't end with Q" )    		
    	}
    }
    
    def _invoke_(name: String)(args: Any*) = {
      error("unsupported")
    }
    
    override def typed[T] : T = {   	
    	x match { 
    	  case Some( v ) =>   v.asInstanceOf[T]
    	  case _ => null.asInstanceOf[T]
        }
    }
    override def toString = "Dynamic(" + x.toString + ")"
  }
  
  implicit  def toDynamic( x :Any ): Dynamic = new DynamicImpl( x.asInstanceOf[AnyRef] )
 

  class Person( adr :Adress ){
	  def adress = adr
  }
  
  class Adress( c :City ){
	  def city = c
  }
  
  class City( c :String ){
	  def code = c
  }
  
  def getCode( p :Dynamic ){
	 println( p.adressQ.cityQ.codeQ.typed[String] ) 
  }
  
  def main( args :Array[String] ){
	
	 val p1 = new Person( new Adress( new Ort( "1111" ) ) )
	 val p2 = new Person( null ) with Elvis
	  
	 getCode( p1 )
	 getCode( p2 )
	 
	 println( p2.adressQ.ortQ.codeQ.typed[String] )
  }
  
}