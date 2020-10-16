//----------------------------  parameter_handler.h  ---------------------------
//    $Id: parameter_handler.h,v 1.1 2004/09/14 00:53:31 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  parameter_handler.h  ---------------------------
#ifndef __deal2__parameter_handler_h
#define __deal2__parameter_handler_h


#include <base/config.h>
#include <base/exceptions.h>

#include <map>
#include <vector>
#include <string>


// public classes; to be declared below
class ParameterHandler;
class MultipleParameterLoop;



// forward declaration
class LogStream;



/**
 * Declare some regexps which
 * may be used to define patterns.
 */ 
namespace Patterns
{
				   /**
				    * Base class to declare common
				    * interface. The purpose of this
				    * class is mostly to define the
				    * interface of patterns, and to
				    * force derived classes to have a
				    * <tt>clone</tt> function. It is thus,
				    * in the languages of the "Design
				    * Patterns" book (Gamma et al.), a
				    * "prototype".
                                    */
  class PatternBase
  {
    public:
				       /**
					* Make destructor of this and all
					* derived classes virtual.
					*/
      virtual ~PatternBase ();
	
				       /**
					* Return <tt>true</tt> if the given string
					* matches the pattern.
					*/
      virtual bool match (const std::string &test_string) const = 0;
	
				       /**
					* Return a string describing the
					* pattern.
					*/
      virtual std::string description () const = 0;
	
				       /**
					* Return a pointer to an
					* exact copy of the
					* object. This is necessary
					* since we want to store
					* objects of this type in
					* containers, were we need
					* to copy objects without
					* knowledge of their actual
					* data type (we only have
					* pointers to the base
					* class).
					*
					* Ownership of the objects
					* returned by this function
					* is passed to the caller of
					* this function.
					*/
      virtual PatternBase * clone () const = 0;

				       /**
					* Determine an estimate for
					* the memory consumption (in
					* bytes) of this object. To
					* avoid unnecessary
					* overhead, we do not force
					* derivd classes to provide
					* this function as a virtual
					* overloaded one, but rather
					* try to cast the present
					* object to one of the known
					* derived classes and if
					* that fails then take the
					* size of this base class
					* instead and add 32 byte
					* (this value is arbitrary,
					* it should account for
					* virtual function tables,
					* and some possible data
					* elements). Since there are
					* usually not many thousands
					* of objects of this type
					* around, and since the
					* memory_consumption
					* mechanism is used to find
					* out where memory in the
					* range of many megabytes
					* is, this seems like a
					* reasonable approximation.
					*
					* On the other hand, if you
					* know that your class
					* deviates from this
					* assumption significantly,
					* you can still overload
					* this function.
					*/
      virtual unsigned int memory_consumption () const;
  };
    
				   /**
                                    * Test for the string being an
                                    * integer. If bounds are given
                                    * to the constructor, then the
                                    * integer given also needs to be
                                    * withing the interval specified
                                    * by these bounds. Note that
                                    * unlike common convention in
                                    * the C++ standard library, both
                                    * bounds of this interval are
                                    * inclusive; the reason is that
                                    * in practice in most cases, one
                                    * needs closed intervals, but
                                    * these can only be realized
                                    * with inclusive bounds for
                                    * non-integer values. We thus
                                    * stay consistent by always
                                    * using closed intervals.
                                    *
                                    * If the upper bound given to
                                    * the constructor is smaller
                                    * than the lower bound, then the
                                    * infinite interval is implied,
                                    * i.e. every integer is allowed.
                                    *
                                    * Giving bounds may be useful if
                                    * for example a value can only
                                    * be positive and less than a
                                    * reasonable upper bound (for
                                    * example the number of
                                    * refinement steps to be
                                    * performed), or in many other
                                    * cases.
                                    */
  class Integer : public PatternBase
  {
    public:
				       /**
					* Minimal integer value. If
					* the numeric_limits class
					* is available use this
					* information to obtain the
					* extremal values, otherwise
					* set it so that this class
					* understands that all values
					* are allowed.
					*/
      static const int min_int_value;

				       /**
					* Maximal integer value. If
					* the numeric_limits class
					* is available use this
					* information to obtain the
					* extremal values, otherwise
					* set it so that this class
					* understands that all values
					* are allowed.
					*/
      static const int max_int_value;
      
				       /**
					* Constructor. Bounds can be
					* specified within which a
					* valid parameter has to
					* be. If the upper bound is
					* smaller than the lower
					* bound, then the infinite
					* interval is meant. The
					* default values are chosen
					* such that no bounds are
					* enforced on parameters.
					*/
      Integer (const int lower_bound = min_int_value,
	       const int upper_bound = max_int_value);
	
				       /**
					* Return <tt>true</tt> if the
					* string is an integer and
					* its value is within the
					* specified range.
					*/
      virtual bool match (const std::string &test_string) const;

				       /**
					* Return a description of
					* the pattern that valid
					* strings are expected to
					* match. If bounds were
					* specified to the
					* constructor, then include
					* them into this
					* description.
					*/
      virtual std::string description () const;

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;
      
    private:
				       /**
					* Value of the lower
					* bound. A number that
					* satisfies the @ref match
					* operation of this class
					* must be equal to this
					* value or larger, if the
					* bounds of the interval for
					* a valid range.
					*/
      const int lower_bound;

				       /**
					* Value of the upper
					* bound. A number that
					* satisfies the @ref match
					* operation of this class
					* must be equal to this
					* value or less, if the
					* bounds of the interval for
					* a valid range.
					*/
      const int upper_bound;
  };
    
				   /**
                                    * Test for the string being a
                                    * <tt>double</tt>. If bounds are
                                    * given to the constructor, then
                                    * the integer given also needs
                                    * to be withing the interval
                                    * specified by these
                                    * bounds. Note that unlike
                                    * common convention in the C++
                                    * standard library, both bounds
                                    * of this interval are
                                    * inclusive; the reason is that
                                    * in practice in most cases, one
                                    * needs closed intervals, but
                                    * these can only be realized
                                    * with inclusive bounds for
                                    * non-integer values. We thus
                                    * stay consistent by always
                                    * using closed intervals.
                                    *
                                    * If the upper bound given to
                                    * the constructor is smaller
                                    * than the lower bound, then the
                                    * infinite interval is implied,
                                    * i.e. every integer is allowed.
                                    *
                                    * Giving bounds may be useful if
                                    * for example a value can only
                                    * be positive and less than a
                                    * reasonable upper bound (for
                                    * example damping parameters are
                                    * frequently only reasonable if
                                    * between zero and one), or in
                                    * many other cases.
                                    */
  class Double : public PatternBase
  {
    public:
				       /**
					* Minimal double value. If the
					* <tt>std::numeric_limits</tt>
					* class is available use this
					* information to obtain the
					* extremal values, otherwise
					* set it so that this class
					* understands that all values
					* are allowed.
					*/
      static const double min_double_value;
      
				       /**
					* Maximal double value. If the
					* numeric_limits class is
					* available use this
					* information to obtain the
					* extremal values, otherwise
					* set it so that this class
					* understands that all values
					* are allowed.
					*/
      static const double max_double_value;
      
				       /**
					* Constructor. Bounds can be
					* specified within which a
					* valid parameter has to
					* be. If the upper bound is
					* smaller than the lower
					* bound, then the infinite
					* interval is meant. The
					* default values are chosen
					* such that no bounds are
					* enforced on parameters.
					*/
      Double (const double lower_bound = min_double_value,
	      const double upper_bound = max_double_value);
	
				       /**
					* Return <tt>true</tt> if the
					* string is a number and its
					* value is within the
					* specified range.
					*/
      virtual bool match (const std::string &test_string) const;

				       /**
					* Return a description of
					* the pattern that valid
					* strings are expected to
					* match. If bounds were
					* specified to the
					* constructor, then include
					* them into this
					* description.
					*/
      virtual std::string description () const;

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;
	
    private:
				       /**
					* Value of the lower
					* bound. A number that
					* satisfies the @ref match
					* operation of this class
					* must be equal to this
					* value or larger, if the
					* bounds of the interval for
					* a valid range.
					*/
      const double lower_bound;

				       /**
					* Value of the upper
					* bound. A number that
					* satisfies the @ref match
					* operation of this class
					* must be equal to this
					* value or less, if the
					* bounds of the interval for
					* a valid range.
					*/
      const double upper_bound;
  };
    
				   /**
                                    * Test for the string being one
                                    * of a sequence of values given
                                    * like a regular expression. For
                                    * example, if the string given
                                    * to the constructor is
                                    * <tt>"red|blue|black"</tt>, then the
                                    * @ref match function returns
                                    * <tt>true</tt> exactly if the string
                                    * is either "red" or "blue" or
                                    * "black". Spaces around the
                                    * pipe signs do not matter and
                                    * are eliminated.
                                    */
  class Selection : public PatternBase
  {
    public:
				       /**
					* Constructor. Take the
					* given parameter as the
					* specification of valid
					* strings.
					*/
      Selection (const std::string &seq);

				       /**
					* Return <tt>true</tt> if the
					* string is an element of
					* the description list
					* passed to the constructor.
					*/
      virtual bool match (const std::string &test_string) const;

				       /**
					* Return a description of
					* the pattern that valid
					* strings are expected to
					* match. Here, this is the
					* list of valid strings
					* passed to the constructor.
					*/
      virtual std::string description () const;

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;

				       /**
					* Determine an estimate for
					* the memory consumption (in
					* bytes) of this object.
					*/
      unsigned int memory_consumption () const;

    private:
				       /**
					* List of valid strings as
					* passed to the
					* constructor. We don't make
					* this string constant, as
					* we process it somewhat in
					* the constructor.
					*/
      std::string sequence;
  };


                                   /**
                                    * This pattern matches a list of
                                    * comma-separated values each of
                                    * which have to match a pattern
                                    * given to the constructor. With
                                    * to additional parameters, the
                                    * number of elements this list has
                                    * to have can be specified.
                                    */
  class List : public PatternBase
  {
    public:
				       /**
					* Maximal integer value. If
					* the numeric_limits class
					* is available use this
					* information to obtain the
					* extremal values, otherwise
					* set it so that this class
					* understands that all values
					* are allowed.
					*/
      static const unsigned int max_int_value;

      				       /**
					* Constructor. Take the
					* given parameter as the
					* specification of valid
					* elements of the list.
					*
					* The two other arguments can
					* be used to denote minimal
					* and maximal allowable
					* lengths of the list.
					*/
      List (const PatternBase  &base_pattern,
            const unsigned int  min_elements = 0,
            const unsigned int  max_elements = max_int_value);

                                       /**
                                        * Destructor.
                                        */
      virtual ~List ();
      
				       /**
					* Return <tt>true</tt> if the
					* string is a comma-separated
					* list of strings each of
					* which match the pattern
					* given to the constructor.
					*/
      virtual bool match (const std::string &test_string) const;

				       /**
					* Return a description of
					* the pattern that valid
					* strings are expected to
					* match. 
					*/
      virtual std::string description () const;

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;

				       /**
					* Determine an estimate for
					* the memory consumption (in
					* bytes) of this object.
					*/
      unsigned int memory_consumption () const;

                                       /**
                                        * Exception.
                                        */
      DeclException2 (ExcInvalidRange,
                      int, int,
                      << "The values " << arg1 << " and " << arg2
                      << " do not form a valid range.");

    private:
				       /**
					* Copy of the pattern that
					* each element of the list has
					* to satisfy.
					*/
      PatternBase *pattern;

                                       /**
                                        * Minimum number of elements
                                        * the list must have.
                                        */
      const unsigned int min_elements;

                                       /**
                                        * Minimum number of elements
                                        * the list must have.
                                        */
      const unsigned int max_elements;
  };

				   /**
                                    * This class is much like the
                                    * Selection class, but it
                                    * allows the input to be a
                                    * comma-separated list of values
                                    * which each have to be given in
                                    * the constructor
                                    * argument. Alternatively, it
                                    * could be viewed as a
                                    * specialization of the List
                                    * class. For example, if the
                                    * string to the constructor was
                                    * <tt>"ucd|gmv|eps"</tt>, then the
                                    * following would be legal input:
                                    * <tt>eps</tt>, <tt>gmv</tt>. You may give an
                                    * arbitrarily long list of values,
                                    * where there may be as many
                                    * spaces around commas as you
                                    * like.  However, commas are not
                                    * allowed inside the values given
                                    * to the constructor.
                                    */
  class MultipleSelection : public PatternBase
  {
    public:
				       /**
					* Constructor. Take the
					* given parameter as the
					* specification of valid
					* strings.
					*/
      MultipleSelection (const std::string &seq);

				       /**
					* Return <tt>true</tt> if the
					* string is an element of
					* the description list
					* passed to the constructor.
					*/
      virtual bool match (const std::string &test_string) const;

				       /**
					* Return a description of
					* the pattern that valid
					* strings are expected to
					* match. Here, this is the
					* list of valid strings
					* passed to the constructor.
					*/
      virtual std::string description () const;

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;

				       /**
					* Determine an estimate for
					* the memory consumption (in
					* bytes) of this object.
					*/
      unsigned int memory_consumption () const;

				       /**
					* Exception.
					*/
      DeclException1 (ExcCommasNotAllowed,
		      int,
		      << "A comma was found at position " << arg1
		      << " of your input string, but commas are not allowed here.");
	
    private:
				       /**
					* List of valid strings as
					* passed to the
					* constructor. We don't make
					* this string constant, as
					* we process it somewhat in
					* the constructor.
					*/
      std::string sequence;
  };

				   /**
                                    * Test for the string being
                                    * either "true" or "false". This
                                    * is mapped to the Selection
                                    * class.
                                    */
  class Bool : public Selection
  {
    public:
				       /**
					* Constrcuctor.
					*/
      Bool ();

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;
  };
        
				   /**
                                    * Always returns <tt>true</tt> when testing a
                                    * string.
                                    */
  class Anything : public PatternBase
  {
    public:
				       /**
					* Constructor. (Allow for at
					* least one non-virtual
					* function in this class, as
					* otherwise sometimes no
					* virtual table is emitted.)
					*/
      Anything ();

				       /**
					* Return <tt>true</tt> if the
					* string matches its
					* constraints, i.e. always.
					*/
      virtual bool match (const std::string &test_string) const;

				       /**
					* Return a description of
					* the pattern that valid
					* strings are expected to
					* match. Here, this is the
					* string <tt>"[Anything]"</tt>.
					*/
      virtual std::string description () const;

				       /**
					* Return a copy of the
					* present object, which is
					* newly allocated on the
					* heap. Ownership of that
					* object is transferred to
					* the caller of this
					* function.
					*/
      virtual PatternBase * clone () const;
  };
}


/**
 *   The ParameterHandler class provides a standard interface to an input file
 *   which provides at run-time for program parameters such as time step sizes,
 *   geometries, right hand sides etc. The input for the program is given in files,
 *   streams or strings in memory using text like
 *   @verbatim
 *     set Time step size = 0.3
 *     set Geometry       = [0,1]x[0,3]
 *   @endverbatim
 *   Input may be sorted into subsection trees in order to give the input a logical
 *   structure.
 *
 *   
 *   @section ParameterHandlerDeclaration Declaration of entries
 *   
 *   In order to use the facilities of a ParameterHandler object, one first has
 *   to make known the different entries the input file may or may not contain. This
 *   is done in the following way:
 *   @code
 *     ...
 *     ParameterHandler prm;
 *     prm.declare_entry ("Time step size",
 *                       "0.2",
 *                       Patterns::Double(),
 *                       "Some documentation");
 *     prm.declare_entry ("Geometry",
 *                       "[0,1]x[0,1]",
 *                       Patterns::Anything());
 *     ...
 *   @endcode
 *   Each entry is declared using the function declare_entry(). The
 *   first parameter is the name of the entry (in short: the
 *   entry). The second is the default answer to be taken in case the
 *   entry is not specified in the input file. The third parameter is
 *   a regular expression which the input (and the default answer) has
 *   to match.  Several such regular expressions are defined in
 *   Patterns. This parameter can be omitted, in which case it
 *   will default to Patterns::Anything, i.e. a pattern that
 *   matches every input string. The fourth parameter can be used to
 *   document the intent or expected format of an entry; its value is
 *   printed as a comment when writing all entries of a
 *   ParameterHandler object using the print_parameters()
 *   function to allow for easier understanding of a parameter
 *   file. It can be omitted as well, in which case no such
 *   documentation will be printed.
 *
 *   Entries may be located in subsections which form a kind of input tree. For example
 *   input parameters for linear solver routines should be classified in a subsection
 *   named <tt>Linear solver</tt> or any other suitable name. This is accomplished in the
 *   following way:
 *   @code
 *     ...
 *       LinEq eq;
 *       eq.declare_parameters (prm);
 *     ...
 *
 *     void LinEq::declare_parameters (ParameterHandler &prm) {
 *       prm.enter_subsection("Linear solver");
 *       prm.declare_entry ("Solver",
 *                          "CG",
 *		            Patterns::Selection("CG|GMRES|GaussElim"),
 *		            "Name of a linear solver for the inner iteration");
 *       prm.declare_entry ("Maximum number of iterations",
 *                          "20",
 *		            ParameterHandler::RegularExpressions::Integer());
 *       ...
 *       prm.leave_subsection ();
 *     };
 *   @endcode
 *
 *   Subsections may be nested. For example a nonlinear solver may have a linear solver
 *   as member object. Then the function call tree would be something like (if the class
 *   <tt>NonLinEq</tt> has a member variables <tt>eq</tt> of type <tt>LinEq</tt>):
 *   @code
 *     void NonLinEq::declare_parameters (ParameterHandler &prm) {
 *       prm.enter_subsection ("Nonlinear solver");
 *       prm.declare_entry ("Nonlinear method",
 *                          "Newton-Raphson",
 *		            ParameterHandler::RegularExpressions::Anything());
 *       eq.declare_parameters (prm);
 *       prm.leave_subsection ();
 *     };
 *   @endcode
 *
 *   For class member functions which declare the different entries we propose to use the
 *   common name <tt>declare_parameters</tt>. In normal cases this method can be <tt>static</tt> since the
 *   entries will not depend on any previous knowledge. Classes for which entries should
 *   logically be grouped into subsections should declare these subsections themselves. If
 *   a class has two or more member variables of the same type both of which should have
 *   their own parameters, this parent class' method <tt>declare_parameters</tt> is responsible to
 *   group them into different subsections:
 *   @code
 *     void NonLinEq::declare_parameters (ParameterHandler &prm) {
 *       prm.enter_subsection ("Nonlinear solver");
 *       prm.enter_subsection ("Linear solver 1");
 *       eq1.declare_parameters (prm);
 *       prm.leave_subsection ();
 *
 *       prm.enter_subsection ("Linear solver 2");
 *       eq2.declare_parameters (prm);
 *       prm.leave_subsection ();
 *       prm.leave_subsection ();
 *     };	
 *   @endcode
 *
 *
 *   @section ParameterHandlerInput Input files and special characters
 *
 *   For the first example above the input file would look like the following:
 *   @verbatim
 *     ...
 *     subsection Nonlinear solver
 *       set Nonlinear method = Gradient
 *       subsection Linear solver
 *         set Solver                        = CG
 *         set Maxmimum number of iterations = 30
 *       end
 *     end
 *     ...                       # other stuff
 *   @endverbatim
 *   The words <tt>subsection</tt>, <tt>set</tt> and <tt>end</tt> may be either written in lowercase or uppercase
 *   letters. Leading and trailing whitespace is removed, multiple whitespace is condensed into
 *   only one. Since the latter applies also to the name of an entry, an entry name will not
 *   be recognised if in the declaration multiple whitespace is used.
 *
 *   In entry names and values the following characters are not allowed: <tt>\#</tt>, <tt>{</tt>, 
 *   <tt>}</tt>, <tt>|</tt>. Their use is reserved for the MultipleParameterLoop class.
 *   
 *   Comments starting with \# are skipped.
 *   
 *   We propose to use the following
 *   scheme to name entries: start the first word with a capital letter and use lowercase
 *   letters further on. The same applies to the possible entry values to the right of the
 *   <tt>=</tt> sign.
 *
 *   
 *   @section ParameterHandlerReading Reading data from input sources
 *   
 *   In order to read input you can use three possibilities: reading from an <tt>std::istream</tt> object,
 *   reading from a file of which the name is given and reading from a string in memory in
 *   which the lines are separated by <tt>@\n</tt> characters. These possibilites are used as follows:
 *   @code
 *     ParameterHandler prm;
 *     ...
 *     // declaration of entries
 *     ...
 *     prm.read_input (cin);         // read input from standard in,
 *     // or
 *     prm.read_input ("simulation.in");
 *     // or
 *     char *in = "set Time step size = 0.3 \n ...";
 *     prm.read_input_from_string (in);
 *     ...
 *   @endcode
 *   You can use several sources of input successively. Entries which are changed more than
 *   once will be overwritten everytime they are used. It is suggested to let the name of
 *   parameter input end in <tt>.prm</tt>.
 *
 *   You should not try to declare entries using declare_entry() and
 *   enter_subsection() with as yet unknown subsection names after
 *   using read_input(). The results in this case are unspecified.
 *
 *   If an error occurs upon reading the input, error messages are
 *   written to <tt>std::cerr</tt>.
 *
 *   
 *   @section ParameterHandlerAccess Getting entry values out of a ParameterHandler object
 *   
 *   Each class gets its data out of a ParameterHandler object by
 *   calling the get()  member functions like this:
 *   @code
 *      void NonLinEq::get_parameters (ParameterHandler &prm) {
 *       prm.enter_subsection ("Nonlinear solver");
 *       std::string method = prm.get ("Nonlinear method");
 *       eq.get_parameters (prm);
 *       prm.leave_subsection ();
 *     };
 *   @endcode
 *   get() returns the value of the given entry. If the entry was not specified in the input
 *   source(s), the default value is returned. You have to enter and leave subsections
 *   exactly as you did when declaring subsection. You may chose the order in which to
 *   transverse the subsection tree.
 *
 *   It is guaranteed that only entries matching the given regular expression are returned,
 *   i.e. an input entry value which does not match the regular expression is not stored.
 *
 *   You can use get() to retrieve the parameter in text form, get_integer() to get an integer
 *   or get_double() to get a double. You can also use get_bool().
 *   It will cause an internal error if the string could not be 
 *   converted to an integer, double or a bool. This should, though, not
 *   happen if you correctly specified the regular expression for this entry; you should not
 *   try to get out an integer or a double from an entry for which no according regular
 *   expression was set. The internal error is raised through the Assert() macro family
 *   which only works in debug mode.
 *
 *   If you want to print out all user selectable features, use the
 *   print_parameters() function. It is generally a good idea to print all parameters
 *   at the beginning of a log file, since this way input and output are together in
 *   one file which makes matching at a later time easier. Additionally, the function
 *   also print those entries which have not been modified in the input file und are
 *   thus set to default values; since default values may change in the process of
 *   program development, you cannot know the values of parameters not specified in the
 *   input file.
 *   
 *   
 *   @section ParameterHandlerStyle Style guide for data retrieval
 *   
 *   We propose that every class which gets data out of a
 *   ParameterHandler object provides a function named
 *   <tt>get_parameters</tt>. This should be declared
 *   <tt>virtual</tt>. <tt>get_parameters</tt> functions in derived classes
 *   should call the <tt>BaseClass::get_parameters</tt> function.
 *
 *
 *   @section ParameterHandlerLarge Experience with large parameter lists
 *  
 *   Experience has shown that in programs defining larger numbers of parameters (more than,
 *   say, fifty) it is advantageous to define an additional class holding these parameters.
 *   This class is more like a C-style structure, having a large number of variables,
 *   usually public. It then has at least two functions, which declare and parse the
 *   parameters. In the main program, the main class has an object of this parameter class
 *   and delegates declaration and parsing of parameters to this object.
 *
 *   The advantage of this approach is that you can keep out the technical details
 *   (declaration and parsing) out of the main class and additionally don't clutter
 *   up your main class with dozens or more variables denoting the parameters.
 *
 *
 *   @section ParameterHandlerFuture Possible future extensions
 *   
 *   <ul>
 *   <li> Allow long input lines to be broken by appending a backslash character
 *     (just like C macros and shell input).
 *   <li> Provide an <tt>input filename</tt> command for the input file to enable users to put the
 *     most common parameters into separate files.
 *   </ul>
 *
 *
 *   
 *   @section Example Worked Example
 *
 *   This is the code:
 *   @code
 *     #include <iostream>
 *     #include "../include/parameter_handler.h"
 *     
 *     
 *     class LinEq {
 *       public:
 *         static void declare_parameters (ParameterHandler &prm);
 *         void get_parameters (ParameterHandler &prm);
 *       private:
 *         std::string Method;
 *         int    MaxIterations;
 *     };
 *     
 *     
 *     class Problem {
 *       private:
 *         LinEq eq1, eq2;
 *         std::string Matrix1, Matrix2;
 *         std::string outfile;
 *       public:
 *         static void declare_parameters (ParameterHandler &prm);
 *         void get_parameters (ParameterHandler &prm);
 *     };
 *     
 *     
 *     
 *     void LinEq::declare_parameters (ParameterHandler &prm) {
 *                                        // declare parameters for the linear
 *                                        // solver in a subsection
 *       prm.enter_subsection ("Linear solver");
 *       prm.declare_entry ("Solver",
 *                          "CG",
 *                          Patterns::Selection("CG|BiCGStab|GMRES"),
 *                          "Name of a linear solver for the inner iteration");
 *       prm.declare_entry ("Maximum number of iterations",
 *                          "20",
 *                          Patterns::Integer());
 *       prm.leave_subsection ();
 *     };
 *           
 *           
 *     void LinEq::get_parameters (ParameterHandler &prm) {
 *       prm.enter_subsection ("Linear solver");
 *       Method        = prm.get ("Solver");
 *       MaxIterations = prm.get_integer ("Maximum number of iterations");
 *       prm.leave_subsection ();
 *       std::cout << "  LinEq: Method=" << Method << ", MaxIterations=" << MaxIterations << std::endl;
 *     };
 *           
 *           
 *           
 *     void Problem::declare_parameters (ParameterHandler &prm) {
 *                                        // first some global parameter entries
 *       prm.declare_entry ("Output file",
 *                          "out",
 *                          Patterns::Anything(),
 *                          "Name of the output file, either relative to the present"
 *                          "path or absolute");
 *       prm.declare_entry ("Equation 1",
 *                          "Laplace",
 *                          Patterns::Anything(),
 *                          "String identifying the equation we want to solve");
 *       prm.declare_entry ("Equation 2",
 *                          "Elasticity",
 *                          Patterns::Anything());
 *     
 *                                        // declare parameters for the
 *                                        // first equation
 *       prm.enter_subsection ("Equation 1");
 *       prm.declare_entry ("Matrix type",
 *                          "Sparse",
 *                          Patterns::Selection("Full|Sparse|Diagonal"),
 *                          "Type of the matrix to be used, either full,"
 *                          "sparse, or diagonal");
 *       LinEq::declare_parameters (prm);  // for eq1
 *       prm.leave_subsection ();
 *           
 *                                        // declare parameters for the
 *                                        // second equation
 *       prm.enter_subsection ("Equation 2");
 *       prm.declare_entry ("Matrix type",
 *                          "Sparse",
 *                          Patterns::Selection("Full|Sparse|Diagonal"));
 *       LinEq::declare_parameters (prm);  // for eq2
 *       prm.leave_subsection ();
 *     };
 *           
 *           
 *     void Problem::get_parameters (ParameterHandler &prm) {
 *                                        // entries of the problem class
 *       outfile = prm.get ("Output file");
 *     
 *       std::string equation1 = prm.get ("Equation 1"),
 *              equation2 = prm.get ("Equation 2");
 *       
 *                                        // get parameters for the
 *                                        // first equation
 *       prm.enter_subsection ("Equation 1");
 *       Matrix1 = prm.get ("Matrix type");
 *       eq1.get_parameters (prm);         // for eq1
 *       prm.leave_subsection ();
 *           
 *                                        // get parameters for the
 *                                        // second equation
 *       prm.enter_subsection ("Equation 2");
 *       Matrix2 = prm.get ("Matrix type");
 *       eq2.get_parameters (prm);         // for eq2
 *       prm.leave_subsection ();
 *     
 *       std::cout << "  Problem: outfile=" << outfile << std::endl
 *            << "           eq1="     << equation1 << ", eq2=" << equation2 << std::endl
 *            << "           Matrix1=" << Matrix1 << ", Matrix2=" << Matrix2 << std::endl;
 *     };
 *          
 *           
 *           
 *           
 *     void main () {
 *       ParameterHandler prm;
 *       Problem p;
 *           
 *       p.declare_parameters (prm);
 *           
 *                                        // read input from "prmtest.prm"; giving
 *                                        // argv[1] would also be a good idea
 *       prm.read_input ("prmtest.prm");
 *           
 *                                        // print parameters to std::cout as ASCII text
 *       std::cout << std::endl << std::endl;
 *       prm.print_parameters (std::cout, ParameterHandler::Text);
 *           
 *                                        // get parameters into the program
 *       std::cout << std::endl << std::endl
 *                 << "Getting parameters:" << std::endl;
 *       p.get_parameters (prm);
 *
 *                                        // now run the program with these
 *                                        // input parameters
 *       p.do_something ();
 *     };
 *   @endcode
 *
 *   
 *   This is the input file (named "prmtest.prm"):
 *   @verbatim
 *                                 # first declare the types of equations
 *     set Equation 1 = Poisson
 *     set Equation 2 = Navier-Stokes
 *
 *     subsection Equation 1
 *       set Matrix type = Sparse
 *       subsection Linear solver    # parameters for linear solver 1
 *         set Solver                       = Gauss-Seidel
 *         set Maximum number of iterations = 40
 *       end
 *     end
 *
 *     subsection Equation 2
 *       set Matrix type = Full
 *       subsection Linear solver
 *         set Solver                       = CG
 *         set Maximum number of iterations = 100
 *       end
 *     end
 *   @endverbatim
 *
 *   And here is the ouput of the program:
 *   @verbatim
 *     Line 8:
 *         The entry value
 *             Gauss-Seidel
 *         for the entry named
 *             Solver
 *         does not match the given regular expression
 *             CG|BiCGStab|GMRES
 *
 *
 *     Listing of Parameters
 *     ---------------------
 *       set Equation 1  = Poisson  # Laplace
 *       set Equation 2  = Navier-Stokes  # Elasticity
 *       set Output file = out
 *       subsection Equation 1
 *         set Matrix type = Sparse  # Sparse
 *         subsection Linear solver
 *           set Maximum number of iterations = 40  # 20
 *           set Solver                       = CG
 *         end
 *       end
 *       subsection Equation 2
 *         set Matrix type = Full  # Sparse
 *         subsection Linear solver
 *           set Maximum number of iterations = 100  # 20
 *           set Solver                       = CG   # CG
 *         end
 *       end
 *
 *
 *     Getting parameters:
 *       LinEq: Method=CG, MaxIterations=40
 *       LinEq: Method=CG, MaxIterations=100
 *       Problem: outfile=out
 *                eq1=Poisson, eq2=Navier-Stokes
 *                Matrix1=Sparse, Matrix2=Full
 *   @endverbatim
 *
 *   
 *   @section References References
 *
 *   This class is inspired by the <tt>MenuSystem</tt> class of <tt>DiffPack</tt>.
 *
 *   @author Wolfgang Bangerth, October 1997, revised February 1998
  */
class ParameterHandler
{
  private:
				     /**
				      * Inhibit automatic
				      * CopyConstructor.
				      */
    ParameterHandler (const ParameterHandler&);

				     /**
				      * Inhibit automatic
				      * assignment operator.
				      */
    ParameterHandler& operator= (const ParameterHandler&);
    
  public:
				     /**
				      * List of possible output
				      * formats.
				      */
    enum OutputStyle {
					   /**
					    * Write human readable output.
					    */
	  Text,
					   /**
					    * Write paramteters as a
					    * LaTeX table.
					    */
	  LaTeX,
					   /**
					    * Write parameters as an
					    * HTML table.
					    */
	  HTML
    };


    
				     /**
				      * Constructor.
				      */
    ParameterHandler ();

				     /**
				      * Destructor. Declare this only
				      * to have a virtual destructor,
				      * which is safer as we have
				      * virtual functions.  It
				      * actually does nothing
				      * spectacular.
				      */
    virtual ~ParameterHandler ();
    
				     /**
				      * Read input from a stream until
				      * stream returns <tt>eof</tt>
				      * condition or error.
				      *
				      * Return whether the read was
				      * successful.
				      */
    virtual bool read_input (std::istream &input);
    
    				     /**
				      * Read input from a file the
				      * name of which is given.
				      *
				      * Return whether the read was
				      * successful.
				      *
				      * This function will
				      * automatically generate the
				      * requested file with default
				      * values if the file did not
				      * exist.
				      */
    virtual bool read_input (const std::string &filename);
    
    				     /**
				      * Read input from a string in
				      * memory. The lines in memory
				      * have to be separated by <tt>@\n</tt>
				      * characters.
				      *
				      * Return whether the read was
				      * successful.
				      */
    virtual bool read_input_from_string (const char *s);

				     /**
				      * Return status of this object:
				      * <tt>true</tt>=clean or
				      * <tt>false</tt>=error occured.
				      */
    bool ok () const;

				     /**
				      * clear status bit and contents.
				      */
    void clear ();


				     /**
				      * Declare a new entry with name
				      * <tt>entry</tt>, default and for
				      * which any input has to match
				      * the <tt>pattern</tt> (default: any
				      * pattern).
				      *
				      * The last parameter defaulting
				      * to an empty string is used to
				      * add a documenting text to each
				      * entry which will be printed as
				      * a comment when this class is
				      * asked to write out all
				      * declarations to a stream using
				      * the print_parameters()
				      * function.
				      *
				      * The function generates an
				      * exception if the default value
				      * doesn't match the given
				      * pattern. An entry can be
				      * declared more than once
				      * without generating an error,
				      * for example to override an
				      * earlier default value.
				      */
    void declare_entry (const std::string           &entry,
                        const std::string           &default_value,
                        const Patterns::PatternBase &pattern = Patterns::Anything(),
                        const std::string           &documentation = std::string());
    
				     /**
				      * Enter a subsection; if not yet
				      * existent, declare it.
				      */
    void enter_subsection (const std::string &subsection);
    
				     /**
				      * Leave present subsection.
				      * Return <tt>false</tt> if there is
				      * no subsection to leave; <tt>true</tt>
				      * otherwise.
				      */
    bool leave_subsection ();

				     /**
				      * Return value of entry
				      * <tt>entry_string</tt>.  If the
				      * entry was changed, then the
				      * changed value is returned,
				      * otherwise the default
				      * value. If the value of an
				      * undeclared entry is required,
				      * an empty string is returned
				      * and <tt>assert</tt> is used to
				      * check whether this entry was
				      * declared (therefore an
				      * exception may be thrown).
				      */
    const std::string & get (const std::string &entry_string) const;
    
				     /**
				      * Return value of entry
				      * <tt>entry_string</tt> as <tt>long int</tt>.
				      */
    long int       get_integer (const std::string &entry_string) const;
    
				     /**
				      * Return value of entry
				      * <tt>entry_string</tt> as
				      * <tt>double</tt>.
				      */
    double         get_double (const std::string &entry_string) const;

				     /**
				      * Return value of entry
				      * <tt>entry_string</tt> as <tt>bool</tt>.
				      */
    bool           get_bool (const std::string &entry_string) const;

				     /**
				      * Print all parameters with the
				      * given style to
				      * <tt>out</tt>. Presently only
				      * <tt>Text</tt> and <tt>LaTeX</tt> are
				      * implemented.
				      *
				      * In <tt>Text</tt> format, the output
				      * is formatted in such a way
				      * that it is possible to use it
				      * for later input again. This is
				      * most useful to record the
				      * parameters for a specific run,
				      * since if you output the
				      * parameters using this function
				      * into a log file, you can
				      * always recover the results by
				      * simply copying the output to
				      * your input file.
				      *
				      * Besides the name and value of
				      * each entry, the output also
				      * contains the default value of
				      * entries if it is different
				      * from the actual value, as well
				      * as the documenting string
				      * given to the
				      * declare_entry() function
				      * if available.
				      */
    std::ostream & print_parameters (std::ostream      &out,
				     const OutputStyle  style);

				     /**
				      * Print out the parameters of
				      * the present subsection as
				      * given by the
				      * <tt>subsection_path</tt> member
				      * variable. This variable is
				      * controlled by entering and
				      * leaving subsections through
				      * the enter_subsection() and
				      * leave_subsection()
				      * functions.
				      *
				      * In most cases, you will not
				      * want to use this function
				      * directly, but have it called
				      * recursively by the previous
				      * function.
				      */
    void print_parameters_section (std::ostream       &out,
				   const OutputStyle   style,
				   const unsigned int  indent_level);

				     /**
				      * Print parameters to a
				      * logstream.  This function
				      * allows to print all parameters
				      * into a log-file.  Sections
				      * will be indented in the usual
				      * log-file style.
				      */
    void log_parameters (LogStream& out);

				     /**
				      * Log parameters in the present
				      * subsection. The subsection is
				      * determined by the
				      * <tt>subsection_path</tt> member
				      * variable. This variable is
				      * controlled by entering and
				      * leaving subsections through
				      * the enter_subsection() and
				      * leave_subsection()
				      * functions.
				      *
				      * In most cases, you will not
				      * want to use this function
				      * directly, but have it called
				      * recursively by the previous
				      * function.
				      */
    void log_parameters_section (LogStream& out);    

				     /**
				      * Determine an estimate for
				      * the memory consumption (in
				      * bytes) of this
				      * object.
				      */
    unsigned int memory_consumption () const;

				     /**
				      * Exception
				      */
    DeclException1 (ExcEntryAlreadyExists,
		    std::string,
		    << "The following entry already exists: " << arg1);
				     /**
				      * Exception
				      */
    DeclException2 (ExcDefaultDoesNotMatchPattern,
		    std::string, std::string,
		    << "The default string <" << arg1
		    << "> does not match the given pattern <" << arg2 << ">");  
				     /**
				      * Exception
				      */
    DeclException0 (ExcAlreadyAtTopLevel);
    				     /**
				      * Exception
				      */
    DeclException1 (ExcEntryUndeclared,
		    std::string,
		    << "You can't ask for entry <" << arg1 << "> you have not yet declared");  
    				     /**
				      * Exception
				      */
    DeclException1 (ExcConversionError,
		    std::string,
		    << "Error when trying to convert the following string: " << arg1);
    
  private:
				     /**
				      * ok bit
				      */
    bool status;

				     /**
				      * Whatever is in a section:
				      * map of entry names together with
				      * entry content and regexp, and
				      * list of subsections.
				      */
    struct Section 
    {
	~Section ();

        struct EntryContent
        {
            std::string            value;
            std::string            documentation;
            Patterns::PatternBase *pattern;
        };
        
                                         /**
                                          * Typedef for a type
                                          * describing all the entries
                                          * in a subsection: this is a
                                          * map from the entry keys to
                                          * a pair of values, one for
                                          * the default string and one
                                          * describing the pattern
                                          * that the entry must match.
                                          */
	typedef
        std::map<std::string, EntryContent>
        EntryType;

                                         /**
                                          * List of entries for this
                                          * section.
                                          */
	EntryType                       entries;

                                         /**
                                          * List of subsections of
                                          * this section.
                                          */
	std::map<std::string, Section*> subsections;

					 /**
					  * Determine an estimate for
					  * the memory consumption (in
					  * bytes) of this
					  * object. Since sometimes
					  * the size of objects can
					  * not be determined exactly
					  * (for example: what is the
					  * memory consumption of an
					  * STL <tt>std::map</tt> type with a
					  * certain number of
					  * elements?), this is only
					  * an estimate. however often
					  * quite close to the true
					  * value.
					  */
	unsigned int memory_consumption () const;
    };

				     /**
				      * Path of presently selected
				      * subsections; empty list means
				      * top level
				      */
    std::vector<std::string> subsection_path;

				     /**
				      * List of default values
				      * organized as a tree of
				      * subsections
				      */
    Section defaults;
    
				     /**
				      * Analogue list of changed
				      * entries. The tree of
				      * subsections is there even if
				      * there are no changed entry
				      * values in a subsection;
				      * therefore enter_subsection()
				      * has to create the tree in both
				      * <tt>Defaults</tt> and
				      * <tt>changed_entries</tt>.
				      */
    Section changed_entries;

				     /**
				      * Scan one line of input.
				      * <tt>lineno</tt> is the number of
				      * the line presently scanned
				      * (for the logs if there are
				      * messages). Return <tt>false</tt> if
				      * line contained stuff that
				      * could not be understood, the
				      * uppermost subsection was to be
				      * left by an <tt>END</tt> or <tt>end</tt>
				      * statement, a value for a
				      * non-declared entry was given
				      * or teh entry value did not
				      * match the regular
				      * expression. <tt>true</tt> otherwise.
				      *
				      * The function modifies its
				      * argument, but also takes it by
				      * value, so the caller's
				      * variable is not changed.
				      */
    bool scan_line (std::string         line,
		    const unsigned int  lineno);

				     /**
				      * Get a pointer to the
				      * <tt>Section</tt> structure in the
				      * <tt>Defaults</tt> tree for the
				      * subsection we are presently
				      * in.
				      */
    Section*       get_present_defaults_subsection ();
    
				     /**
				      * Same, <tt>const</tt> version.
				      */ 
    const Section* get_present_defaults_subsection () const;

				     /**
				      * Get a pointer to the Section structure
				      * in the <tt>changed_entries</tt> tree
				      * for the subsection we are presently in.
				      */
    Section* get_present_changed_subsection ();
    
    				     /**
				      * Same, <tt>const</tt> version.
				      */
    const Section* get_present_changed_subsection () const;

    friend class MultipleParameterLoop;
};



/**
 *   The class MultipleParameterLoop offers an easy possibility to test several
 *   parameter sets during one run of the program. For this it uses the
 *   ParameterHandler class to read in data in a standardized form, searches for
 *   variant entry values and performs a loop over all combinations of parameters.
 *
 *   Variant entry values are given like this:
 *   @verbatim
 *     set Time step size = { 0.1 | 0.2 | 0.3 }
 *   @endverbatim
 *   The loop will then perform three runs of the program, one for each value
 *   of <tt>Time step size</tt>, while all other parameters are as specified or with their
 *   default value. If there are several variant entry values in the input a loop is
 *   performed for each combination of variant values:
 *   @verbatim
 *     set Time step size = { 0.1 | 0.2 }
 *     set Solver         = { CG  | GMRES }
 *   @endverbatim
 *   will result in four runs of the programs, with time step 0.1 and 0.2 for each
 *   of the two solvers.
 *
 *   Opposite to a variant entry, an array entry looks like this:
 *   @verbatim
 *     set Output file = ofile.{{ 1 | 2 | 3 | 4 }}
 *   @endverbatim
 *   This indicates that if there are variant entries producing a total of four
 *   different runs will write their results to the files <tt>ofile.1</tt>, <tt>ofile.2</tt>,
 *   <tt>ofile.3</tt> and <tt>ofile.4</tt>, respectively. Array entries do not generate multiple
 *   runs of the main loop themselves, but if there are variant entries, then in
 *   the <i>n</i>th run of the main loop, also the <i>n</i>th value of an array is returned.
 *
 *   Since the different variants are constructed in the order of declaration, not in
 *   the order in which the variat entries appear in the input file, it may be
 *   difficult to guess the mapping between the different variants and the appropriate
 *   entry in an array. You will have to check the order of declaration, or use
 *   only one variant entry.
 *   
 *   It is guaranteed that only selections which match the regular expression given
 *   upon declaration of an entry are given back to the program. If a variant value
 *   does not match the regular expression, the default value is stored and an error
 *   is issued. Before the first run of the loop, all possible values are checked
 *   for their conformance, so that the error is issued at the very beginning of the
 *   program.
 *
 *   
 *   @section ParameterHandlerUsage Usage
 *   
 *   The usage of this class is similar to the ParameterHandler class. First the
 *   entries and subsections have to be declared, then a loop is performed in which
 *   the different parameter sets are set, a new instance of a user class is created
 *   which is then called. Taking the classes of the example for the
 *   ParameterHandler class, the extended program would look like this:
 *   @code
 *     class HelperClass : public MultipleParameterLoop::UserClass {
 *       public:
 *         HelperClass ();
 *                     
 *         virtual void create_new (unsigned int runNo);
 *         virtual void declare_parameters (ParameterHandler &prm); 
 *         virtual void run (ParameterHandler &prm);
 *       private:
 *         Problem *p;
 *     };
 *           
 *           
 *     HelperClass::HelperClass () : p(0) {};
 *           
 *           
 *     void HelperClass::create_new (unsigned int runNo) {
 *       if (p) delete p;
 *       p = new Problem;
 *     };
 *                    
 *           
 *     void HelperClass::declare_parameters (ParameterHandler &prm) {
 *           				   // entries of the problem class
 *             			   // note: must be static member!
 *       Problem::declare_parameters (prm);
 *     };
 *           
 *           
 *     void HelperClass::run (ParameterHandler &prm) {
 *       p->get_parameters (prm);
 *     };
 *               
 *           
 *           
 *     void main () {
 *                                        // don't know why we have to write
 *     				   // "class" here, but it won't work
 *     				   // otherwise
 *       class MultipleParameterLoop prm;
 *       HelperClass h;
 *           
 *       h.declare_parameters (prm);
 *       prm.read_input ("prmtest.prm");
 *       prm.loop (h);
 *     };    
 *   @endcode
 *         
 *   As can be seen, first a new helper class has to be set up. This must contain
 *   a virtual constructor for a problem class. You can also derive your problem
 *   class from MultipleParameterLoop::UserClass and let <tt>create_new</tt> clear all
 *   member variables. If you have access to all inherited member variables in
 *   some way this is the recommended procedure. A third possibility is to use
 *   multiple inheritance and derive a helper class from both the
 *   MultipleParameterLoop::UserClass and the problem class. In any case,
 *   <tt>create_new</tt> has to provide a clean problem object which is the problem in
 *   the second and third possibility. However, if possible, the second way should
 *   be chosen.
 *
 *   The derived class also
 *   has to provide for member functions which declare the entries and which run
 *   the program. Running the program includes getting the parameters out of the
 *   ParameterHandler object.
 *
 *   After defining an object of this helper class and an object of the
 *   MultipleParameterLoop class, the entries have to be declared in the same way
 *   as for the ParameterHandler class. Then the input has to be read. Finally
 *   the loop is called. This executes the following steps:
 *   @code
 *     for (each combination)
 *       {
 *         UserObject.create_new (runNo);
 *
 *  set parameters for this run
 *  
 *         UserObject.run (*this);
 *       };
 *   @endcode
 *   <tt>UserObject</tt> is the parameter to the <tt>loop</tt> function. <tt>create_new</tt> is given the number
 *   of the run (starting from one) to enable naming output files differently for each
 *   run.
 *
 *   
 *   @section ParameterHandlerUsageSyntax Syntax for variant and array entry values
 *   
 *   Variant values are specified like <tt>prefix{ v1 | v2 | v3 | ... }postfix</tt>. Whitespace
 *   to the right of the opening brace <tt>{</tt> is ignored as well as to the left of the
 *   closing brace <tt>}</tt> while whitespace on the respectively other side is not ignored.
 *   Whitespace around the mid symbols <tt>|</tt> is also ignored. The empty selection
 *   <tt>prefix{ v1 | }postfix</tt> is also allowed and produces the strings <tt>prefixv1postfix</tt> and
 *   <tt>prefixpostfix</tt>.
 *
 *   The syntax for array values is equal, apart from the double braces:
 *   <tt>prefix{{ v1 | v2 | v3 }}postfix</tt>.
 *   
 *
 *   @section ParameterHandlerExample Worked example
 *   
 *   Given the above extensions to the example program for the ParameterHandler and the
 *   following input file
 *   @verbatim
 *     set Equation 1 = Poisson
 *     set Equation 2 = Navier-Stokes
 *     set Output file= results.{{ 1 | 2 | 3 | 4 | 5 | 6 }}
 *
 *     subsection Equation 1
 *       set Matrix type = Sparse
 *       subsection Linear solver 
 *         set Solver                       = CG
 *         set Maximum number of iterations = { 10 | 20 | 30 }
 *       end
 *     end
 *
 *     subsection Equation 2
 *       set Matrix type = Full
 *       subsection Linear solver
 *         set Solver                       = { BiCGStab | GMRES }
 *         set Maximum number of iterations = 100
 *       end
 *     end
 *   @endverbatim
 *   this is the output:
 *   @verbatim
 *     LinEq: Method=CG, MaxIterations=10
 *     LinEq: Method=BiCGStab, MaxIterations=100
 *     Problem: outfile=results.1
 *              eq1=Poisson, eq2=Navier-Stokes
 *              Matrix1=Sparse, Matrix2=Full
 *     LinEq: Method=CG, MaxIterations=20
 *     LinEq: Method=BiCGStab, MaxIterations=100
 *     Problem: outfile=results.2
 *              eq1=Poisson, eq2=Navier-Stokes
 *              Matrix1=Sparse, Matrix2=Full
 *     LinEq: Method=CG, MaxIterations=30
 *     LinEq: Method=BiCGStab, MaxIterations=100
 *     Problem: outfile=results.3
 *              eq1=Poisson, eq2=Navier-Stokes
 *              Matrix1=Sparse, Matrix2=Full
 *     LinEq: Method=CG, MaxIterations=10
 *     LinEq: Method=GMRES, MaxIterations=100
 *     Problem: outfile=results.4
 *              eq1=Poisson, eq2=Navier-Stokes
 *              Matrix1=Sparse, Matrix2=Full
 *     LinEq: Method=CG, MaxIterations=20
 *     LinEq: Method=GMRES, MaxIterations=100
 *     Problem: outfile=results.5
 *              eq1=Poisson, eq2=Navier-Stokes
 *              Matrix1=Sparse, Matrix2=Full
 *     LinEq: Method=CG, MaxIterations=30
 *     LinEq: Method=GMRES, MaxIterations=100
 *     Problem: outfile=results.6
 *              eq1=Poisson, eq2=Navier-Stokes
 *              Matrix1=Sparse, Matrix2=Full
 *   @endverbatim
 *   Since <tt>create_new</tt> gets the number of the run it would also be possible to output
 *   the number of the run.
 *   
 *   
 *   @section ParameterHandlerReferences References
 *   This class is inspired by the <tt>Multipleloop</tt> class of <tt>DiffPack</tt>.
 *
 *   @author Wolfgang Bangerth, October 1997
 */
class MultipleParameterLoop : public ParameterHandler
{
  public:
				     /**
				      * This is the class the helper class or the
				      * problem class has to be derived of.
				      */
    class UserClass 
    {
      public:
					 /**
					  * <tt>create_new</tt> must provide a clean
					  * object, either by creating a new one
					  * or by cleaning an old one.
					  */
	virtual void create_new (const unsigned int runNo) = 0;
	
					 /**
					  * This should declare parameters and call
					  * the <tt>declare_parameters</tt> function of the
					  * problem class.
					  */
	virtual void declare_parameters (ParameterHandler &prm) = 0;
	
					 /**
					  * Get the parameters and run any
					  * necessary action.
					  */
	virtual void run (ParameterHandler &prm) = 0;
    };

				     /**
				      * Constructor
				      */
    MultipleParameterLoop ();

				     /**
				      * Destructor. Declare this only to have
				      * a virtual destructor, which is safer
				      * as we have virtual functions.
				      * It actually does nothing spectacular.
				      */
    virtual ~MultipleParameterLoop ();

    				     /**
				      * Read input from a stream until stream
				      * returns <tt>eof</tt> condition or error.
				      */
    virtual bool read_input (std::istream &Input);
    
    				     /**
				      * Read input from a file the name of which
				      * is given.
				      */
    virtual bool read_input (const std::string &FileName);
    
    				     /**
				      * Read input from a string in memory. The
				      *  lines in memory have to be separated by
				      *  <tt>@\n</tt> characters.
				      */
    virtual bool read_input_from_string (const char *s);

				     /**
				      * run the central loop.
				      */
    void loop (UserClass &uc);

				     /**
				      * Determine an estimate for
				      * the memory consumption (in
				      * bytes) of this
				      * object.
				      */
    unsigned int memory_consumption () const;

  private:

				     /**
				      * An object in the list of entries with
				      * multiple values.
				      */
    class Entry 
    {
      public:
                                         /**
                                          * Declare what a multiple entry
                                          * is: a variant * entry (in
                                          * curly braces <tt>{</tt>, <tt>}</tt>) or an
                                          * array (in double curly braces
                                          * <tt>{{</tt>, <tt>}}</tt>).
                                          */
        enum MultipleEntryType 
        {
              variant, array
        };

					 /**
					  * Constructor
					  */
	Entry () {};
	
					 /**
					  * Construct an object with given subsection
					  * path, name and value. The splitting up
					  * into the different variants is done
					  * later by <tt>split_different_values</tt>.
					  */
	Entry (const std::vector<std::string> &Path,
	       const std::string              &Name,
	       const std::string              &Value);

					 /**
					  * Split the entry value into the different
					  * branches.
					  */
	void split_different_values ();

					 /**
					  * Path to variant entry.
					  */
	std::vector<std::string> subsection_path;

					 /**
					  * Name of entry.
					  */
	std::string         entry_name;

					 /**
					  * Original variant value.
					  */
	std::string         entry_value;
	
					 /**
					  * List of entry values constructed out of
					  * what was given in the input file (that
					  * is stored in EntryValue.
					  */
	std::vector<std::string> different_values;

					 /**
					  * Store whether this entry is a variant
					  * entry or an array.
					  */
	MultipleEntryType      type;

					 /**
					  * Determine an estimate for
					  * the memory consumption (in
					  * bytes) of this
					  * object.
					  */
	unsigned int memory_consumption () const;
    };

				     /**
				      * List of variant entry values.
				      */
    std::vector<Entry> multiple_choices;
    
				     /**
				      * Number of branches constructed
				      * from the different
				      * combinations of the variants.
				      * This obviously equals the
				      * number of runs to be
				      * performed.
				      */
    unsigned int n_branches;

				     /**
				      * Initialize the different
				      * branches, i.e.  construct the
				      * combinations.
				      */
    void init_branches ();
    
				     /**
				      * Initialize the branches in the
				      * given section.
				      */
    void init_branches_section (const ParameterHandler::Section &sec);
    
				     /**
				      * Transfer the entry values for one run
				      * to the entry tree.
				      */
    void fill_entry_values (const unsigned int run_no);
};


/*------------------------------ Inline functions ------------------------------*/

inline
bool
ParameterHandler::ok() const
{
  return status;
}


#endif
