import scala.io.Source
import scala.math
import java.io._

object apPlusPlus {
  
  val reserved = Array("var", "const" ,"print","and","or","not","if", "then", "else", "endif", "while", "do","Let","")
  val delimiters = Array("*", "/", "+", "-", "=", "<", ">", "==", "><", ",", ":",";","'","\"")
    
  val reserved_count=reserved.length; 
  val delimiters_count=delimiters.length;
  
  def main(args:Array[String]){
    
     // create an empty map
   val int_variables = scala.collection.immutable.Map[String, Int]()
   val alpha_variables = scala.collection.immutable.Map[String, String]()
   val bool_variables = scala.collection.immutable.Map[String, String]()
   
   //for const values
   val const_int_variables = scala.collection.immutable.Map[String, Int]()
   val const_alpha_variables = scala.collection.immutable.Map[String, String]()
   val const_bool_variables = scala.collection.immutable.Map[String, String]()
    
    val code:String =  Source.fromFile("fileopen4.appp").mkString //making a string of the file
    val text= code.replace("\n", "").replace("\r", ""); //ending new line in our code string
    val word=text.split(" ") //creating a string array on basis of spaces
    next(word,word.length,0,int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables) //calling our next function
        
  }
  
  
    //traverses word by word, till the end of our code 
    def next(w:Array[String],length:Int,count:Int,int_variables:Map[String,Int],
        alpha_variables:Map[String,String],bool_variables:Map[String,String],const_int_variables:Map[String,Int],
        const_alpha_variables:Map[String,String],const_bool_variables:Map[String,String]):Unit={
     if (count<length)
     {
        if (found_reserved(w(count),0)==true)
        { //println("Found reserved word: "+w(count))
          if (w(count)=="print") //like print x ;
           {
            if (bool_variables.contains(w(count+1)))
            {
              print(bool_variables(w(count+1))+"\n")
              
            }
            else if(alpha_variables.contains(w(count+1)))
            {
                print(alpha_variables(w(count+1))+"\n")
            }
            else if(int_variables.contains(w(count+1)))
            {
              print(int_variables(w(count+1))+"\n")
            }
            else if (const_bool_variables.contains(w(count+1)))
            {
              print(const_bool_variables(w(count+1))+"\n")
            }
            else if (const_alpha_variables.contains(w(count+1)))
            {
               print(const_alpha_variables(w(count+1))+"\n")
            }
            else if (const_int_variables.contains(w(count+1)))
            {
              print(const_int_variables(w(count+1))+"\n")
            }
            else if (w(count+1) == "\"" || w(count+1) == "'") // if we pass in string in " " or ' ' 
            {
              val c=print_string(w,count+2,2)
              println()
              next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
              const_alpha_variables,const_bool_variables)
              return //exit function 
            }
            else { //if variable does not exist
               println("Error in print: No such variable exists: "+ w(count+1)) 
               return //exit function 
            }
              next(w,length,count+2,int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)  
           }
          else if (w(count)=="if") //for if statements
          {
            
             if (bool_found(bool_variables,const_bool_variables,w(count+1))==true && w(count+2)=="then") //for bool 
            {
              if (get_bool(bool_variables,const_bool_variables,w(count+1))==true)
              {//go to statement inside if block 
              next(w,length,count+3,int_variables,alpha_variables,bool_variables,const_int_variables,
              const_alpha_variables,const_bool_variables) 
              }
              else 
              {//skips statement inside if block
                val c = find_else(w,count,0)
                next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
                const_alpha_variables,const_bool_variables) 
              }
            }
            else if (condition_statement(w,length,count,int_variables,alpha_variables,bool_variables,const_int_variables,
                       const_alpha_variables,const_bool_variables)=="true" && w(count+4)=="then") //for integers
            {
              //go to statement inside if block 
              next(w,length,count+5,int_variables,alpha_variables,bool_variables,const_int_variables,
              const_alpha_variables,const_bool_variables)
            }
            else if (condition_statement(w,length,count,int_variables,alpha_variables,bool_variables,const_int_variables,
                       const_alpha_variables,const_bool_variables)=="false" && w(count+4)=="then")
            {
              //skip the statement inside if block
              val c = find_else(w,count,0)
              next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
                const_alpha_variables,const_bool_variables) 
            }
            else 
            {
              println("Error in if statement !")
              return //exit function
            }
          }
          else if (w(count)=="else") //for else statements
          {
            //skip the statement inside else block
            //println("in else statement")
              val c = find_end(w,count,0)
              next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
              const_alpha_variables,const_bool_variables) 
          }
          else if (w(count)=="while") //for while statements
          {
            //println ("before checking while condition")
             if (bool_found(bool_variables,const_bool_variables,w(count+1))==true && w(count+2)=="do") //while x do
            {
              // println ("if x do ")
              if (get_bool(bool_variables,const_bool_variables,w(count+1))==true)
              {
                //println("goes inside while")
               //go to statement inside while block 
              next(w,length,count+3,int_variables,alpha_variables,bool_variables,const_int_variables,
              const_alpha_variables,const_bool_variables) 
              }
              else 
              {
                //println("has to skip while")
                //skips statement inside while block
                val c = find_end(w,count,0)
                next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
                const_alpha_variables,const_bool_variables) 
                return //insert new return 
              }
            }
           else if (condition_statement(w,length,count,int_variables,alpha_variables,bool_variables,const_int_variables,
                       const_alpha_variables,const_bool_variables)=="true" )
            {
             // println("goes inside while")
              //go to statement inside while block 
              next(w,length,count+5,int_variables,alpha_variables,bool_variables,const_int_variables,
              const_alpha_variables,const_bool_variables)
              return //insert new return 
            }
            else if (condition_statement(w,length,count,int_variables,alpha_variables,bool_variables,const_int_variables,
                       const_alpha_variables,const_bool_variables)=="false")
            {
              //println("has to skip while")
              //skip the statement inside while block
              val c = find_end(w,count,0)
              next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
                const_alpha_variables,const_bool_variables) 
                return //insert new return 
            }
            else 
            {
               println("Error in while statement ! "+ w(count))
               return //exit function  
            }  
          }
          else if (w(count)=="")//for end of while loop
          {
             val c = go_to_while(w,count,0)
             next(w,length,count+c,int_variables,alpha_variables,bool_variables,const_int_variables,
             const_alpha_variables,const_bool_variables) 
             return //insert new return 
          } 
          else if (w(count)=="const") //for const variables declaration
          {
             if (w(count+3)=="int") //for conditions like const x:int
          {
             if (w(count+4)=="=")//for conditions like const x:int=1
             {
               val new_const_int_variables= const_int_variables +(w(count+1)->w(count+5).toInt) 
               next(w,length,count+6,int_variables,alpha_variables,bool_variables,new_const_int_variables,
        const_alpha_variables,const_bool_variables)
             }
             if (w(count+4)==";")//for conditions like const x:int;
             {
                val new_const_int_variables= const_int_variables + (w(count+1)->"0".toInt);
                next(w,length,count+5,int_variables,alpha_variables,bool_variables,new_const_int_variables,
        const_alpha_variables,const_bool_variables)
             }    
          }
          else if (w(count+3)=="alpha")
          {
             if (w(count+4)=="=")//for conditions like const x:alpha='hello'
             {
             val new_const_alpha_variables=const_alpha_variables+(w(count+1)->w(count+6))
             next(w,length,count+8,int_variables,alpha_variables,bool_variables,const_int_variables,
        new_const_alpha_variables,const_bool_variables)
             }
             if (w(count+4)==";")//for conditions like const x:alpha;
             {
                val new_const_alpha_variables=const_alpha_variables+(w(count+1)->"null")
                next(w,length,count+5,int_variables,alpha_variables,bool_variables,const_int_variables,
        new_const_alpha_variables,const_bool_variables)
             }
          }
          else if (w(count+3)=="bool") 
          {
              if (w(count+4)=="=")//for conditions like const x:bool=tt
              {
                val new_const_bool_variables=const_bool_variables+(w(count+1)->w(count+5))
                next(w,length,count+6,int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,new_const_bool_variables)
              }
               if (w(count+4)==";")//for conditions like const x:bool;
              {
                val new_const_bool_variables=const_bool_variables+(w(count+1)->"ff")
                next(w,length,count+5,int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,new_const_bool_variables)
              }
          }
          else 
          {
            println("Error at: "+w(count)+" "+w(count+1)+" "+w(count+2)+" "+w(count+3)+" ")
            return //exit 
          }
          }
          else if (w(count)=="var")
          {
             if (w(count+3)=="int")
          {
            if (w(count+4)=="=")//for conditions like var x:int=1
             {
               val new_int_variables= int_variables +(w(count+1)->w(count+5).toInt) 
               next(w,length,count+6,new_int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
             }
             if (w(count+4)==";")//for conditions like var x:int;
             {
                val new_int_variables= int_variables + (w(count+1)->"0".toInt);
                next(w,length,count+5,new_int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
             }
          }
          else if (w(count+3)=="alpha")
          {
             if (w(count+4)=="=")//for conditions like var x:alpha='hello'
             {
             val new_alpha_variables=alpha_variables+(w(count+1)->w(count+6))
             next(w,length,count+8,int_variables,new_alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
             }
             if (w(count+4)==";")//for conditions like var x:alpha;
             {
                val new_alpha_variables=alpha_variables+(w(count+1)->"null")
                next(w,length,count+5,int_variables,new_alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
             }
          }
          else if (w(count+3)=="bool")
          {
             if (w(count+4)=="=")//for conditions like var x:bool=tt
              {
                val new_bool_variables=bool_variables+(w(count+1)->w(count+5))
                next(w,length,count+6,int_variables,alpha_variables,new_bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
              }
               if (w(count+4)==";")//for conditions like var x:bool;
              {
                val new_bool_variables=bool_variables+(w(count+1)->"ff")
                next(w,length,count+5,int_variables,alpha_variables,new_bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
              }
          }
          else
          {
           println("Error at: "+w(count)+" "+w(count+1)+" "+w(count+2)+" "+w(count+3)+" ")
           return //exit  
          }   
          }
          else
           next(w,length,count+1,int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
        }
        else if (found_delimiter(w(count),0)==true) //look for delimiter
         {
          //println("Found delimiter: "+w(count))
          next(w,length,count+1,int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
         }
        else //if neither reserved or delimiter, then look for variables
        {
          //println("Found variable: "+w(count))
          if (w(count+1)=="=") 
           {
             for ((k,v) <- int_variables) //for conditions like x=0;
             {
               if (k==w(count) && w(count+3)==";") //if variable has been declared earlier, it is assigned a value
                 {val new_int_variables=int_variables-w(count)+(w(count)->w(count+2).toInt)
                 next(w,length,count+4,new_int_variables,alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
                 }             
             }
              for ((k,v) <- alpha_variables) //for conditions like x='hello';
             {
               if (k==w(count) && w(count+5)==";") //if variable has been declared earlier, it is assigned a value
                 {val new_alpha_variables=alpha_variables-w(count)+(w(count)->w(count+3))
                 next(w,length,count+6,int_variables,new_alpha_variables,bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
                 }             
             }
             for ((k,v) <- bool_variables) //for conditions like x=tt;
             {
               if (k==w(count) && w(count+3)==";") //if variable has been declared earlier, it is assigned a value
                 {val new_bool_variables=bool_variables-w(count)+(w(count)->w(count+2))
                 next(w,length,count+4,int_variables,alpha_variables,new_bool_variables,const_int_variables,
        const_alpha_variables,const_bool_variables)
                 }             
             }
              for ((k,v) <- const_int_variables) //for conditions like x=0;
             {
               if (k==w(count)&& (w(count+3)==";" || found_operator(w(count+3))==true)) //if variable has been declared earlier, it can't be assigned a value
                 {
                 println(w(count)+": is a const variable, value can't be assigned !")
                 return //exit
                 }             
             }
              for ((k,v) <- const_alpha_variables) //for conditions like x='hello'
             {
               if (k==w(count)&& (w(count+5)==";" || found_operator(w(count+3))==true)) //if variable has been declared earlier, it can't be assigned a value
                 {
                  println(w(count)+": is a const variable, value can't be assigned !")
                  return //exit
                 }             
             }
             for ((k,v) <- const_bool_variables) //for conditions like x=tt;
             {
               if (k==w(count)&& (w(count+3)==";" || found_operator(w(count+3))==true)) //if variable has been declared earlier, it can't be assigned a value
                 {
                  println(w(count)+": is a const variable, value can't be assigned !")
                  return //exit
                 }             
             }
             
             if (int_found(int_variables,const_int_variables,w(count))==true)//if the int variable is existing
             {
               //println(w(count)+" "+count+ "in main function")
                if (w(count+2)=="-" && w(count+4)==";") //for conditions like x=-y;
                  {
                   if (int_found(int_variables,const_int_variables,w(count+3))==true || isInt(w(count+3))==true)
                   {
                     if (const_int_found(const_int_variables,w(count))==false) //if not a const value 
                     {
                     //println("No const variable found ")
                     //var not_int_variable = 0 - w(count+3).toInt;
                     val not_int_variable= 0 - get_int(int_variables,const_int_variables,w(count+3)); 
                       
                     val new_int_variables=int_variables-w(count)+(w(count)->not_int_variable)
                     next(w,length,count+5,new_int_variables,alpha_variables,bool_variables,const_int_variables,
                     const_alpha_variables,const_bool_variables)
                     }
                     else 
                     {
                        println(w(count)+": is a const variable, value can't be assigned !")
                        return //exit
                     }
                   }
                   else 
                   {
                     println("Error: No such variable exists : "+ w(count+3)) 
                     return //exit function 
                   }
                  }
                else 
                {
                   multi_operation_int(w,length,count,int_variables,alpha_variables,bool_variables,const_int_variables,
                   const_alpha_variables,const_bool_variables)
                   //1st we'll call the multi_operation function 
                   return //exit function
                }
             } 
             
             if (bool_found(bool_variables,const_bool_variables,w(count))==true)//if the bool variable is existing
             {
               //println(w(count)+" "+count+ "in main function")
                if (w(count+2)=="not" && w(count+4)==";") //for conditions like x=not y;
                  {
                  //println("Not operation")
                   if (bool_found(bool_variables,const_bool_variables,w(count+3))==true)
                   {
                      //println("Found bool variable")
                       if (const_bool_found(const_bool_variables,w(count))) //if not a const value 
                     {
                          println(w(count+3)+": is a const variable, value can't be assigned !")
                          return //exit 
                     }
                     else 
                     {
                        //println("Not a const variable")
                     if(get_bool(bool_variables,const_bool_variables,w(count+3))==true)
                       {
                       //println("In if condition")
                     
                       val new_bool_variables=bool_variables-w(count)+(w(count)->"ff")
                       next(w,length,count+5,int_variables,alpha_variables,new_bool_variables,const_int_variables,
                       const_alpha_variables,const_bool_variables)
                       }
                     else
                        {
                       //println("In else condition")
                      
                       val new_bool_variables=bool_variables-w(count)+(w(count)->"tt")
                       next(w,length,count+5,int_variables,alpha_variables,new_bool_variables,const_int_variables,
                       const_alpha_variables,const_bool_variables)
                       }
                   
                     }
                   }
                   else 
                   {
                     println("Error: No such variable exists: "+ w(count+3)) 
                     return //exit function 
                   }
                  }
             else
             {
              multi_operation_bool(w,length,count,int_variables,alpha_variables,bool_variables,const_int_variables,
               const_alpha_variables,const_bool_variables)
               //1st we'll call the multi_operation function 
               return //exit function
             }
             }  
            
           }
          else { //if variable does not exist
            
            println("Error: No such variable exists: "+ w(count)) 
           return //exit function  
          }
        }
      }
     else 
       return //exit function
    }
    
    //finds the word in the reserved Array
    def found_reserved(r:String,c:Int):Boolean={
      if (c<reserved_count)
      {
        if (r.equals(reserved(c)))
          return true
        else
          found_reserved(r,c+1)
      }
      else
        return false
    }
    
    //finds the operator/symbol in the delimiters Array
    def found_delimiter(d:String,c:Int):Boolean={
      if (c<delimiters_count)
      {
        if (d.equals(delimiters(c)))
          return true
        else
          found_delimiter(d,c+1)
      }
      else
        return false
    }   
    
    //checks for operator
     def found_operator(d:String):Boolean={
        if (d=="+")
          return true
        else  if (d=="-")
          return true
        else  if (d=="*")
          return true
        else  if (d=="/")
          return true
        else  if (d=="^")
          return true
        else  if (d=="and")
          return true
        else  if (d=="or")
          return true
        else  if (d=="not")
          return true 
        else
        return false
    }   
    
    //checks it an integer has been entered 
     def isInt(num:String) :Boolean = {
       var c = num.toInt
       if (c<0)
       return true
       else if (c>=0)
         return true
       else 
         return false
     }
     
     //prints the strings inside " " or ' ' 
     def print_string( w:Array[String],count:Int,c:Int):Int={
      if (w(count)=="\"" || w(count)=="'")
        return c +1
      else{
        print(w(count)+" ")
        print_string(w,count+1,c+1)
      }     
     }
    
    //checks if int variable is available
    def int_found(int_variables:Map[String,Int],const_int_variables:Map[String,Int],s:String):Boolean={
      for ((k,v) <- int_variables) 
             {
               if (k==s) //if variable is found 
                 {
                 return true
                 }             
             }
      
      
      for ((k,v) <- const_int_variables) 
             {
               if (k==s) //if variable is found
                 {
                 return true
                 }             
             }

      return false;
    }
    
     //checks if bool variable is available
    def bool_found(bool_variables:Map[String,String],const_bool_variables:Map[String,String],s:String):Boolean={
      for ((k,v) <- bool_variables) 
             {
               if (k==s) //if variable is found 
                 {
                 return true
                 }             
             }
      
      
      for ((k,v) <- const_bool_variables) 
             {
               if (k==s) //if variable is found
                 {
                 return true
                 }             
             }
      
      return false;
    }
    
    
     //checks if alpha variable is available
    def alpha_found(alpha_variables:Map[String,String],const_alpha_variables:Map[String,String],s:String):Boolean={
      for ((k,v) <- alpha_variables) 
             {
               if (k==s) //if variable is found 
                 {
                 return true
                 }             
             }
      
      
      for ((k,v) <- const_alpha_variables) 
             {
               if (k==s) //if variable is found
                 {
                 return true
                 }             
             }
      
      return false;
    }
    
    
     
     //checks if const int variable is available
    def const_int_found(const_int_variables:Map[String,Int],s:String):Boolean={ 
      for ((k,v) <- const_int_variables) 
             {
               if (k==s) //if variable is found
                 {
                 return true
                 }             
             }
      
      return false;
    }
    
     //checks if const bool variable is available
    def const_bool_found(const_bool_variables:Map[String,String],s:String):Boolean={ 
      for ((k,v) <- const_bool_variables) 
             {
               if (k==s) //if variable is found
                 {
                 return true
                 }             
             }
      
      return false;
    }
    
    
    //returns an int value 
    def get_int(int_variables:Map[String,Int],const_int_variables:Map[String,Int],s:String):Int={
      
      for ((k,v) <- int_variables) 
             {
               if (k==s) //if variable is found 
                 {
                 return v
                 }             
             }
      
      
      for ((k,v) <- const_int_variables) 
             {
               if (k==s) //if variable is found
                 {
                 return v
                 }             
             }
      
      var c = s.toInt;
  
      return c;
    }
    
     //returns an bool value 
    def get_bool(bool_variables:Map[String,String],const_bool_variables:Map[String,String],s:String):Boolean={
      
      for ((k,v) <- bool_variables) 
             {
               if (k==s) //if variable is found 
                 {
                 if (v=="tt")
                 return true
                 else 
                   return false
                 }             
             }
      
      
      for ((k,v) <- const_bool_variables) 
             {
               if (k==s) //if variable is found
                 {
                 if (v=="tt")
                 return true
                 else 
                   return false
                 }             
             }
  
      return false;
    }
    
    //performs operation
    def int_operation(result:Int,w:Array[String],count:Int,int_variables:Map[String,Int],const_int_variables:Map[String,Int]):Int={
        if (w(count+1)==";")
          return result;
        else 
        {
         // println(w(count)+" in int_operation")
          if (int_found(int_variables,const_int_variables,w(count+2))==true || isInt(w(count+2))==true) //if there is an existing variable 
          {
           val num2=get_int(int_variables,const_int_variables,w(count+2));
         if (w(count+1).equals("+"))
         {
          val result1= (result+num2).toInt
          int_operation(result1,w,count+2,int_variables,const_int_variables)
         }
        else  if (w(count+1).equals("-"))
        {
           val result1= (result - num2).toInt
           int_operation(result1,w,count+2,int_variables,const_int_variables)
        }
        else  if (w(count+1).equals("*"))
        {
           val result1= (result * num2).toInt
           int_operation(result1,w,count+2,int_variables,const_int_variables)
        }
        else  if (w(count+1).equals("/"))
        {
           val result1= (result/num2).toInt
           int_operation(result1,w,count+2,int_variables,const_int_variables)
        }
        else  if (w(count+1).equals("^"))
          {
           val result1 = (scala.math.pow(result.toDouble,num2.toDouble)).toInt
           int_operation(result1,w,count+2,int_variables,const_int_variables)
          }
        else
        {
          println("Error: No such operator exists: "+ w(count+1)) 
          return -10101011; //if no operator found
        }
         }
        else
        { println("Error: No such variable exists: "+ w(count+2)) 
        return -10101010; //if variable does not exist 
        }
        }
    }
    
    
    //performs operation
    def bool_operation(result:Boolean,w:Array[String],count:Int,bool_variables:Map[String,String],const_bool_variables:Map[String,String]):String={
        if (w(count+1)==";")
        {
          if (result == true) 
            return "tt"
          else
            return "ff"
        }
        else 
        {
         // println(w(count)+" in int_operation")
          if (bool_found(bool_variables,const_bool_variables,w(count+2))==true) //if there is an existing variable 
          {
           val num2=get_bool(bool_variables,const_bool_variables,w(count+2));
         if (w(count+1).equals("and"))
         {
          val result1= (result & num2)
          bool_operation(result1,w,count+2,bool_variables,const_bool_variables)
         }
        else  if (w(count+1).equals("or"))
        {
           val result1= (result | num2)
           bool_operation(result1,w,count+2,bool_variables,const_bool_variables)
        }
        else  if (w(count+1).equals("^"))
        {
           val result1= (result ^ num2)
           bool_operation(result1,w,count+2,bool_variables,const_bool_variables)
        }
        else
        {
          println("Error: No such operator exists: "+ w(count+1)) 
          return "no op"; //if no operator found
        }
         }
        else
        { println("Error: No such variable exists: "+ w(count+2)) 
        return "no var"; //if variable does not exist 
        }
        }
    }
    
    //get end of expression
    def get_end(w:Array[String],count:Int,acc:Int):Int={
      if (w(count)==";")
        return acc
      else
      {
        get_end(w,count+2,acc+2)
      }
    }
    
    //function for multi operations for int 
    def multi_operation_int(w:Array[String],length:Int,count:Int,int_variables:Map[String,Int],
        alpha_variables:Map[String,String],bool_variables:Map[String,String],const_int_variables:Map[String,Int],
        const_alpha_variables:Map[String,String],const_bool_variables:Map[String,String]):Unit={
      // println("Hello in multi_operation")
               if (int_found(int_variables,const_int_variables,w(count+2))==true || isInt(w(count+2))==true) //if there is an existing variable 
                 { 
                   var num1=get_int(int_variables,const_int_variables,w(count+2)); 
                 //  println(num1+"in multi operation")
                       var result=int_operation(num1,w,count+2,int_variables,const_int_variables) //perform operation
                       // println(result+" here is result")
                         if (result == -10101011)
                           return
                         else if (result == -10101010)
                           return
                         else
                         {
                         var new_int_variables= int_variables - w(count) + (w(count)->result)
                         var acc=get_end(w,count+3,0)
                         next(w,length,count+acc+3,new_int_variables,alpha_variables,bool_variables,const_int_variables,
                         const_alpha_variables,const_bool_variables)
                         return //once this stack ends 
                         }
                 }
               else 
               {
                 println("Error: No such variable exists: "+ w(count+2)) 
                // return //exit function  
               }
    }
    
    //function for multi operations for bool
    def multi_operation_bool(w:Array[String],length:Int,count:Int,int_variables:Map[String,Int],
        alpha_variables:Map[String,String],bool_variables:Map[String,String],const_int_variables:Map[String,Int],
        const_alpha_variables:Map[String,String],const_bool_variables:Map[String,String]):Unit={
       //println("Hello in multi_operation")
               if (bool_found(bool_variables,const_bool_variables,w(count+2))==true) //if there is an existing variable 
                 { 
                   var num1=get_bool(bool_variables,const_bool_variables,w(count+2)); 
                 //  println(num1+"in multi operation")
                       var result=bool_operation(num1,w,count+2,bool_variables,const_bool_variables) //perform operation
                       // println(result+" here is result")
                         if (result == "no op")
                           return
                         else if (result == "no var")
                           return
                         else
                         {
                         var new_bool_variables= bool_variables - w(count) + (w(count)->result)
                         var acc=get_end(w,count+3,0)
                         next(w,length,count+acc+3,int_variables,alpha_variables,new_bool_variables,const_int_variables,
                         const_alpha_variables,const_bool_variables)
                         return //once this stack ends 
                         }
                 }
               else 
               {
                 println("Error: No such variable exists: "+ w(count+2)) 
               //  return //exit function  
               }
    }
    
    //checks the status of the condition
    def condition_statement(w:Array[String],length:Int,count:Int,int_variables:Map[String,Int],
        alpha_variables:Map[String,String],bool_variables:Map[String,String],const_int_variables:Map[String,Int],
        const_alpha_variables:Map[String,String],const_bool_variables:Map[String,String]):String={
      
      if (int_found(int_variables,const_int_variables,w(count+1))==true) //if there is an existing variable
      {
        if (int_found(int_variables,const_int_variables,w(count+3))==true || isInt(w(count+3))==true) //if there is an existing variable and condition like x == y 
        {
          if (condition_operator_found(w(count+2))==true) 
          {
            val num1=get_int(int_variables,const_int_variables,w(count+1));
            val num2=get_int(int_variables,const_int_variables,w(count+3));
             if (w(count+2) == "==") // x == y 
             {
               if (num1==num2)
               {
                 return "true"
               }
               else 
                 return "false"
             }
            else if (w(count+2) == ">") // x > y
             {
              if (num1 > num2)
              {
                 return "true"
              }
               else 
                 return "false"
             }
            else if (w(count+2) == "<") // x < y
             {
              if (num1 < num2)
              {
               //println("true")
                 return "true"
              }
               else   
               {
                 //println("false")
                 return "false"
               }
             }
            else // x >< y
            {
              if (num1 != num2)
              {
                 return "true"
              }
               else 
                 return "false"
            }
          }
          else
          {
             println("Error: Unidentified conditional operator: "+ w(count+2)) 
             return "stop" //exit function 
          }
      }
        else 
        {
           println("Error: No such int variable exists: "+ w(count+3)) 
           return "stop" //exit function 
        }
     }
      
      else if (bool_found(bool_variables,const_bool_variables,w(count+1))==true) //if there is an existing variable
      {
        if (bool_found(bool_variables,const_bool_variables,w(count+3))==true) //if there is an existing variable and condition like x == y 
        {
          if (condition_operator_found(w(count+2))==true) 
          {
            val num1=get_bool(bool_variables,const_bool_variables,w(count+1));
            val num2=get_bool(bool_variables,const_bool_variables,w(count+3));
             if (w(count+2) == "==") // x == y 
             {
               if (num1==num2)
               {
                 return "true"
               }
               else 
                 return "false"
             }
            else if (w(count+2) == ">") // x > y
             {
              println("Error: Unidentified conditional operator: "+ w(count+2)) 
              return "stop" //exit function 
             }
            else if (w(count+2) == "<") // x < y
             {
              println("Error: Unidentified conditional operator: "+ w(count+2)) 
              return "stop" //exit function 
             }
            else // x >< y
            {
              if (num1 != num2)
              {
                 return "true"
              }
               else 
               {
                 //println("false")
                 return "false"
               }
            }
          }
          else 
          {
            println("Error: Unidentified conditional operator: "+ w(count+2)) 
            return "stop" //exit function  
          } 
        }
         else 
         {
         println("Error: No such bool variable exists: "+ w(count+3)) 
         return "stop" //exit function  
         }   
     
       }
      
      else if (alpha_found(alpha_variables,const_alpha_variables,w(count+1))==true) //if there is an existing variable
      {
        if (alpha_found(alpha_variables,const_alpha_variables,w(count+3))==true) //if there is an existing variable and condition like x == y 
        {
          if (condition_operator_found(w(count+2))==true) 
          {
             if (w(count+2) == "==") // x == y 
             {
               if (w(count+1)==w(count+3))
               {
                 return "true"
               }
               else 
                 return "false"
             }
            else if (w(count+2) == ">") // x > y
             {
              println("Error: Unidentified conditional operator: "+ w(count+2)) 
              return "stop" //exit function 
             }
            else if (w(count+2) == "<") // x < y
             {
              println("Error: Unidentified conditional operator: "+ w(count+2)) 
              return "stop" //exit function 
             }
            else // x >< y
            {
              if (w(count+1) != w(count+3))
              {
                 return "true"
              }
               else 
                 return "false"
            }
          }
          else 
          {
            println("Error: Unidentified conditional operator: "+ w(count+2)) 
            return "stop" //exit function  
          } 
        }
         else 
         {
         println("Error: No such alpha variable exists: "+ w(count+3)) 
         return "stop" //exit function  
         }   
     
       }
      
      //incorrect variable 
       else 
      {
         println("Error: No such variable exists: "+ w(count+1)) 
         return "stop" //exit function  
    }  
    }
    
    
    //checks if conditional operator is correct or not 
    def condition_operator_found(op:String):Boolean={
      if (op == "==")
        return true
      else if (op == ">")
        return true
      else if (op == "<")
        return true
      else if (op == "><")
        return true
      else 
        return false 
    }
      
    //finds else and returns the count
    def find_else(w:Array[String],count:Int,acc:Int):Int={
      if (w(count)=="else")
        return acc +1
      else 
        find_else(w,count+1,acc+1)
    }
    
     //finds end of else and returns the count
    def find_end(w:Array[String],count:Int,acc:Int):Int={
      //println(acc)
      //println(w(count))
      if (w(count)=="")
      {
        return acc +1
      }
      else 
        find_end(w,count+1,acc+1)
    }
    
    
    //go back to while loop 
    def go_to_while(w:Array[String],count:Int,acc:Int):Int={
      if (w(count)=="while")
        return -(acc)
      else 
        go_to_while(w,count-1,acc+1)
    }

}
