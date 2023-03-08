+++
title = "C++11: An Introduction to Some of the Fun New Additions"
description = "A pretty comprehensive introduction to a large proportion of the new features of C++11. This is to help refresh my mind as much as any, but it's a good place to start, and full of examples."
date = 2013-04-11
[extra]
created = "2013-04-10"
toc = 2
+++

The new standard in C++ - known as C++11 - has bought a host of cool new improvements to the language. I've spent the last week or two getting back into the language myself, which has included a lot of reading about and playing with the new additions. As such, I thought I'd write up everything I learned along the way.

This post turned out a lot longer than I was planning, and still does not cover everything, but I reckon I have hit on most of the more fun additions at the very least. It's worth noting that all of the following examples compile just fine on gcc 4.7.2. For a more complete rundown of the new additions, [this][wiki] article seems to cover everything.

# The `auto` Keyword

A lifesaver when long variable types are used. When a variable is first initialized, you can now forgo explicitly telling the compiler what type it is, and let it deduce the type itself by using the `auto` keyword instead. Here is a standard loop using iterators:

```cpp
//create a new vector (using the new list initialization):
std::vector<double> vec{10.0, 20.0, 30.0, 40.0};

//iterate over it:
for(std::vector<double>::iterator it = vec.begin();
	it != vec.end(); it++)
	{
	std::cout << *it << std::endl;
	}
```

Typing out long variable types leads to a potential source of error, and can also diminish readbility. Utilising the `auto` keyword:

```cpp
//create a new vector (using the new list initialization):
std::vector<double> vec{10.0, 20.0, 30.0, 40.0};

//iterate over it:
for(auto it = vec.begin(); it != vec.end(); it++)
	{
	std::cout << *it << std::endl;
	}
```

Much cleaner!

## `decltype()`

The `decltype()` function returns the resulting type of the expression passed in to it. It can thus be used in place of the type in some expression. It is useful when the type is otherwise not known, for example when template parameters are involved. A couple of simple examples first:

```cpp
//set type of i to result of the expression 5+3 (int):
decltype(5+3) i = 5+3;

auto t1 = makeSomeObject1();
auto t2 = makeSomeObject2();
//set type of t3 to whatever the type of t1+t2 is:
decltype(t1+t2) t3 = t1+t2;
```

This becomes far more useful when dealing with template functions in which the return type depends on the input types, and is unknown beforehand. Previously, the return type may have had to be explicitly declared as a template parameter. However, using the new alternative function syntax provided in c++11, we can overcome this problem:

```cpp
//old syntax:
template<typename R, typename T, typename U>
R doSomething(T a, U b)
	{
	return a+b;
	}
...
doSomething<double>(3.0, 5);

//new syntax:
template<typename T, typename U>
auto doSomethingBetter(T a, U b) -> decltype(a+b)
	{
	return a+b;
	}
...
doSomethingBetter(3.0, 5);

```

Note that the `auto` keyword, in this format, has a different meaning from that described above; it basically tells the compiler that the return type will be stated after the input. This allows us to take the input variable types into account, as by the time the return type is provided, they are known. As such, the following won't work, as the input variables are not known about at the point that we attempt to use them to calculate the return type:

```cpp
//error, as a and b aren't known when the return type is given:
template<typename T, typename U>
decltype(a+b) doSomethingBetter(T a, U b)
	{
	return a+b;
	}
```

# Range for loops

Even using the auto keyword, standard for loops are more verbose than they need to be when we want to iterate over an entire collection, which is often the case.

Present in most other languages, range for loops are a welcome addition to c++11. Taking our earlier iterator example, we can see the difference here:

```cpp
//old way to iterate over a container (called vec):
for(auto it = vec.begin(); it != vec.end(); it++)
	{
	//print each value in vec:
	std::cout << *it << std::endl;
	}

//new way, using a range for loop:
for(auto i : vec)
	{
	std::cout << i << std::endl;
	}

//get a reference to i if we want to change the value:
for(auto & i : vec)
	{
	//add 10 to each item in the vector:
	i += 10;
	}
```

As a bonus, these range based for loops also work with standard arrays:

```cpp
//declare array of 5 values:
int a[5]{10,20,30,40,50};

//add 10 to each value in array:
for(auto & val : a)
	{
	val += 10;
	}
```

As an aside, c++11 also adds the standalone `begin()` and `end()` functions, which return iterators to the beginning and end of a container just as the member functions `.begin()` and `.end()` do. The advantage of the standalone functions is that they also work with arrays, allowing you to write more generic code when explicitly using iterators too.

Remember to explicitly ask for references to the values in the array if you wish to modify them or avoid copying.

# The `using` keyword for aliases

The using keyword can replace the `typedef` keyword entirely, and provides a more readable way of defining aliases to things:

```cpp
typedef std::vector<int> vInt;
using vInt = std::vector<int>;

typedef unsigned int UINT;
using UINT = unsigned int;

typedef struct{int i = 0; double d = 2;} THING;
using THING = struct{int i = 0; double d = 2;};
```

## Template `using`

The using keyword also introduces something that typedef could never do; you can alias template functions with using statements, as in the following:

```cpp
#include <vector>

template<typename T> using v = std::vector<T>;
//v<T> is now an alias for std::vector<T>

int main()
    {
    v<int> t1{1,3,5,7,9};
    }
```

# Lambda Functions

Lambda functions are a very cool addition to the language. Essentially, the new syntax allows you to declare anonymous functions when needed, which can help keep related code in the same place, and save effort when you are passing functions in to other functions. Here is the usual way of sorting a vector of custom structs by some value as an example:

```cpp
struct MyStruct
    {
    int a;
    std::string b;
    };

int compareA(const MyStruct & s1,const MyStruct & s2)
    {
    return s1.a < s2.a;
    }

int main()
    {
    std::vector<MyStruct> vec{ {10, "Hello"}, {8, "Yes"}, {15, "No"} };
    //remember to include the algorithm header for std::sort.
    std::sort(vec.begin(), vec.end(), compareA);
    return 0;
    }
```

Using lambda, we can avoid defining a compare function and do everything inline instead:

```cpp
struct MyStruct
    {
    int a;
    std::string b;
    };

int main()
    {
    std::vector<int> vec{5,1,3,4,2};
    std::sort(vec.begin(), vec.end(), [](const MyStruct & s1,const MyStruct & s2)
		 {
		 return s1.a < s2.a;
		 });
	 return 0;
    }
```

the square brackets `[]` denote the beginning of a lambda function. Next, we provide the arguments that it takes, and finally declare the function. Normally, the return type can be automatically deduced, though you can apply the new return syntax to be explicit:

```cpp
//assign variable f to a new lambda function:
auto f = [](int a, int b)->double {return a+b;};

//use it:
std::cout << f(7, 10) << std::endl;
```

## Passing local variables in to a lambda function

Lambda functions cannot, by default, see any of the variables declared in the local scope. However, it is simple to _capture_ the variables such that they can be used in the lambda expression:

```cpp
int some_number = 7;
int another_number = 12;

//won't work, as the above variables are unknown inside the lambda:
auto f = []()
	{
	return some_number + another_number;
	};

//captures references to all local variables:
auto f2 = [&]()
	{
	//change value of some_number:
	some_number = 100;
	return some_number + another_number;
	};

//captures copies of all local variables:
auto f2 = [=]()
	{
	//value won't change outside lambda function now:
	some_number = 100;
	return some_number + another_number;
	};

//capture copy of another number, and reference to some_number:
auto f2 = [another_number,&some_number]()
	{
	//...
	};

//capture reference to some_number and copy of everything else:
auto f2 = [=,&some_number]()
	{
	//...
	};
```

If you use a lambda function inside a member function of a class, you'll need to capture the `this` pointer in order to access the other member variables of that class. Doing so will give you automatic access to member variables and functions:

```cpp
struct Simple
    {
    int a = 1;
    int doSomething()
        {
        auto f = [this]()
            {
            return a + 10;
            };
        return f();
        }
    };
```

At this point, it's also worth noting that the `std::function` object can be used to easily pass around functions, whether they are lambda functions, regular functions, or classes with overloaded parentheses that act as functions:

```cpp
#include <iostream>
#include <functional>

int func(std::string in) { return in.length(); }

struct func_class
    {
    int operator() (std::string a)
        {
        return a.length();
        }
    };

int main()
    {
    //take in regular function:
    std::function<int(std::string)> f(func);
    std::cout << f("hello") << std::endl;

    //take in lambda function:
    std::function<double(int,int)> f2 = [](int a, int b)
        {
        return a+b;
        };
    std::cout << f2(10, 30) << std::endl;

    //take in function object:
    func_class temp_func;
    std::function<int(std::string)> f3(temp_func);
    std::cout << f3("a long string") << std::endl;
    }
```

The template argument for `std::function` takes its parameter in the form `return_type(input_type, input_type..)`, allowing you to specify the function signature it will be used for.

This provides a simple, standardized way to accept functions which match the given signature as parameters to some other function, or declare lambda functions as local variables (though the auto keyword can be used here too for simplicity).

# Multi-threading

C++11 finally brings in standardised support for multi-threading, which is very cool, and pretty easy to start using too. The current options are the low level `std::thread` class, and the high level wrapper `std::async`, which simplifies exception handling and return values. You'll need to include the `thread` header for these goodies.

## `std::thread`

To start a new thread, we first pass some function into the `std::thread` constructor (using lambda syntax or regular function syntax). At some point after this, we can then either `join()` the constructed thread with its calling thread, or `detach()` it. Joining it to the current thread blocks the current thread until it has finished executing. Detaching it allows it to run independent of the scope in which it was created. The main thread must be kept alive until any child threads finish; once the main thread returns, the program exits.

It is worth noting that using the raw `std::thread` is not advised; better to wrap it in a containing class which takes care of exception handling and thread destruction (by joining or detaching it when it goes out of scope for instance). Once you understand the basics you can figure out for yourself how best to put them to practise (something I will need to do myself). A "Hello World" example using a new thread:

```cpp
#include <thread>
#include <iostream>

void doSomething()
    {
    std::cout << "Hello World" << std::endl;
    }

int main()
    {
    std::thread t1(doSomething);
    t1.join();
    return 0;
    }
```

This simply prints `Hello World!` in a separate thread, joins it back to the main thread (which blocks the main thread from continuing until the newly created thread finishes), and exits. We can make it more interesting by firing off multiple threads:

```cpp
#include <thread>
#include <vector>
#include <iostream>

void doSomething()
    {
    std::cout << "Hello World" << std::endl;
    }

int main()
    {
    std::vector<std::thread> threads;
    for(int i = 0; i < 5; i++)
        {
        threads.push_back(std::thread(doSomething));
        }
    for(auto& t : threads)
        {
        t.join();
        }
    return 0;
    }
```

Notice that we place the threads into a vector, so that we can later call join on them all. This ensures that they all finish before the program hits the end of the main function and terminates. Any arguments that we want to pass into a function we're sending off to another thread can be added as additional arguments when the thread is created:

```cpp
void doSomething(std::string s, int val)
    {
    std::cout << s << " with val: " << val << std::endl;
    //outputs "Boo with val: 20"
    }

int main()
    {
    std::thread t1(doSomething, "Boo", 20);
    t1.join();
    return 0;
    }
```

If you output some text using multiple threads, you'll see that the order in which output is printed is often somewhat garbled. This is because there are multiple threads trying to simultaneously write to std::cout.

## std::mutex and std::lock_guard

The solution to garbled output, and various other issues arising from multiple threads trying to work with some variable at the same time, is to force the threads to behave sequentially for parts of the function.

One way to force threads to behave sequentailly when necessary is to use a `mutex` (you'll need to include the mutex header for these). When a mutex lock is called in one thread, the other threads must wait until the mutex is unlocked again before they can continue past the lock:

```cpp
std::mutex m;

void doSomething()
    {
    //do things in parallel here.

    //now, force sequential access after the lock:
    m.lock();
    std::cout << "Hello World" << std::endl;
    //release the lock, allowing another thread in:
    m.unlock();
    }

int main()
    {
    std::vector<std::thread> threads;
    for(int i = 0; i < 5; i++)
        {
        threads.push_back(std::thread(doSomething));
        }
    for(auto& t : threads)
        {
        t.join();
        }
    return 0;
    }
```

After the call to `m.lock()`, subsequent threads that hit that line must wait until `m.unlock()` is called, as which point the next thread takes ownership of the lock, and continues on. Thus, the text output is no longer garbled in the above example. As mutexes force sequential access, they should be used as little as possible to maximise the amount of concurrency that can take place and minimise the time threads are blocked.

If an exception is thrown after a lock has been attained, you must be careful to ensure that the lock is released, otherwise the other threads will be blocked from continuing indefinitely. The convenient `std::lock_guard` wrapper takes a mutex in its initialization and locks it, unlocking it again automatically upon its destruction. This guarantees that a mutex will be unlocked regardless of whether an error is thrown in the thread or not. An example:

```cpp
std::mutex m;
void doSomething()
    {
    //forces sequential access for duration of scope:
    std::lock_guard<std::mutex> lock(m);
    std::cout << "Hello World" << std::endl;
    //no need to explicitly unlock anything.
    }
```

Of course, in proper applications, you could enclose the lock_guard in a new scope so that it releases the lock as soon as it is no longer needed, as in:

```cpp
void doSomething()
    {
    //do stuff in parallel here.

    	{
    	//do stuff in sequence here.
    	std::lock_guard<std::mutex> lock(m);
	   std::cout << "Hello World" << std::endl;
	   }

	 //more stuff in parallel.
    }
```

An alternative to using locks as in the above, is to use atomics, which are atomic versions of the primitive types. What does this mean? well, even something as simple as incrementing an `int` in multiple threads can lead to an unexpected outcome, as the various threads are simultaneously reading the value of the int, modifying that value, and assigning the int to that value. Take the following:

```cpp
int i = 0;

void doSomething()
    {
    //increment i:
    ++i;
    }

int main()
    {
    std::vector<std::thread> threads;
    for(int i = 0; i < 500; i++)
        {
        threads.push_back(std::thread(doSomething));
        }
    for(auto& t : threads)
        {
        t.join();
        }
    //print final value of i:
    std::cout << i << std::endl;
    return 0;
    }
```

You might expect that the value of `i` would always be 500, but it is in fact sometimes lower. By replacing `int i = 0;` above with `std::atomic<int> i(0);` (and including the atomic header), we remove this issue, as each operation applied to the atomic version of a variable is carried out in full by a thread before any other thread can act on it. It's a bit like applying a mutex lock around every operation you carry out on the atomic variable, but from what I gather, faster.

## `std::async` for higher level multi-threading

The `std::async` function works much like the `std::thread` function above in that it can take in a function, and then arguments which are sent to that function. It can also optionally take in a launch policy as its first argument, which forces it to behave as either a separate thread, or not.

One of the nice things about async however is that, without a launch policy, it will decide how many separate threads to spawn, regardless of how many async calls you make, so you don't have to worry about spawning way too many threads for instance. Another nice thing is that the async function simplifies returning values from threads, by returning an `std::future` object upon being called. this object can be asked for the return value, which blocks the thread that asked until the value is returned from the asynchronous function. An example:

```cpp
#include <iostream>
#include <thread>
#include <future>

std::string func(std::string in)
	{
	return in + " returned.";
	}

int main()
    {
    //create future from async call:
    std::future<std::string> ret = std::async(func, "hello");
    //get value from future:
    std::cout << ret.get() << std::endl;
    //outputs "hello returned."
    return 0;
    }
```

# Hash tables

One of the sought after additions to the standard is hash tables. These work very similarly to the existing set and map (and multiset and multimap) containers. The main advantage is that they have a better average time complexity when it comes to adding and retrieving values than the standard set and map, as they are store values by hashing them rather than inserting them into a tree. The disadvantage is that values are not stored in any particular order, whereas the standard map and set class store values ordered by their keys.

It is also worth pointing out that, even with constant time complexity, a complex hashing function and the more complex retrieval can lead to worse performance than the existing options, though they would likely scale better to larger numbers of inputs.

Here is an example of the unordered map (requiring the `unordered_map` header):

```cpp
struct Stuff
    {
    int number;
    std::string word;
    };

int main()
    {
    std::unordered_map<std::string, Stuff> m;

    //add some entries to the map:
    m["first"] = {10, "first one"};
    m["second"] = {20, "second one"};
    //keys are unique, so this overwrites the above:
    m["second"] = {30, "second one again"};

    //retrieve the entries for printing:
    std::cout << m["first"].word << ", " << m["first"].number << std::endl;
    std::cout << m["second"].word << ", " << m["second"].number << std::endl;
    }
```

Here, the key is a string, and the mapped value is a user-defined structure. The exact same thing could have been done with a regular `map`. The `unordered_multimap` allows multiple entries with the same key, and as such requires slightly different syntax to use. An example:

```cpp
struct Stuff
    {
    int number;
    std::string word;
    };

int main()
    {
    std::unordered_multimap<std::string, Stuff> m;

    //create pair on insertion:
    m.emplace("first", Stuff{10, "first one"});
    //initializer list insertion:
    m.insert({ "second", {20, "second one"} });
    //make pair explictly and insert:
    std::pair<std::string,Stuff> tmp = {"second", Stuff{30, "second one again"} };
    m.insert(tmp);

    //print all values in no particular order:
    for(auto & val : m)
        {
        //val is a std::pair containing string and Stuff:
        std::cout << val.first << ": "
                  << val.second.word << ", "
                  << val.second.number << std::endl;
        }

    //print all values of Stuff with the key "second":
    auto r = m.equal_range("second");
    for(auto it = r.first; it != r.second; it++)
        {
        Stuff & item = it->second;
        std::cout << item.number << ", " << item.word << std::endl;
        }
    }
```

Much like these are used like `map` and `multimap`, the `unordered_set` and `unordered_multiset` are used like `set` and `multiset`. In either case, the element that you wish to add is also the key. Elements can be added, and then iterated over or queried to see whether they exist in the set or not. In the case of multisets - as with multimaps - you can also iterate over elements with a specific key. I'll summarize them both with a quick example:

```cpp
//### first, the unordered set ###:
std::unordered_set<std::string> s;

//insert a couple of items:
s.insert("Hello");
s.insert("No way");

//get an iterator to an item (or to s.end if not present):
auto it = s.find("Hello");
if(it != s.end())
  std::cout << *it << " is in the set." << std::endl;

//### now, the unordered_multiset ###:
std::unordered_multiset<std::string> multi_s;

//insert a couple of items:
multi_s.insert("Hello");
multi_s.insert("Hello");
multi_s.insert("No way");
multi_s.insert("Hello");

//count number of times "Hello" was inserted:
auto it_pair = multi_s.equal_range("Hello");
std::cout << "Hello seen "
        << distance(it_pair.first,it_pair.second)
        << " times."
        << std::endl;
```

# New Smart Pointers

Managing dynamic resources solely with the `new` and `delete` operator can be a risky business; if an exception is thrown before `delete` can be called, for instance, the result is a memory leak. In addition, when pointers are returned from functions, it is not always clear whether the resource pointed too by it needs to be deleted or not (in other words, has the ownership been transferred).

C++11 introduces `unique_ptr`, `shared_ptr` and `weak_ptr` as tools to better deal with this memory and ownership management (from the memory header). Each works in a slightly different way:

- A `shared_ptr` can be pointed at any dynamically allocated resource, and holds a reference count which begins at 1. Each time a copy of it is made, the reference count increases, and each time one of these copies is destroyed, the count decreases. When the count hits 0, the underlying resource being pointed to is deleted.
- A `unique_ptr` can also be pointed at any dynamically allocated resource, but cannot be copied or assigned too. Ownership can be transferred using `std::move`, which invalidates the original pointer.
- A `weak_ptr` points to an object managed by a `shared_ptr`, but does not own it. The `weak_ptr` must be temporarily turned into a `shared_ptr` in order to be used: doing so may well reveal that the object no longer exists (the shared_ptr might have deleted it, in which case it is turned into a nullptr.

Here are a couple of examples, starting with the `unique_ptr`:

```cpp
struct Stuff
    {
    int number;
    std::string word;
    };

std::unique_ptr<Stuff> ptr(new Stuff{10, "Hello"});
std::cout << ptr->number << ", " << ptr->word << std::endl;

std::unique_ptr<Stuff> ptr2 = ptr; //ERROR.
std::unique_ptr<Stuff> ptr2(ptr); //ERROR.

//OK, explicitly transfer ownership of ptr, rendering it useless:
std::unique_ptr<Stuff> ptr2 = std::move(ptr);

//outputs true; ptr is now dead:
std::cout << (ptr == nullptr) << std::endl;
```

The `shared_ptr` is similar except it can be created using a custom function, and can be copied and assigned at will:

```cpp
struct Stuff
    {
    int number;
    std::string word;
    Stuff(int n, std::string w): number(n),word(w)
        {}
    };

int main()
    {
        {
        auto ptr1 = std::make_shared<Stuff>(10, "Hello");
        //also allowed but not as efficient:
        //std::shared_ptr<Stuff> ptr1(new Stuff{10, "Hello"});

        //increases reference count, but points to same object:
        auto ptr2 = ptr1;
        auto ptr3(ptr2);

        //two different methods to access object value:
        std::cout << ptr2->number << std::endl;
        std::cout << (*ptr3).word << std::endl;
        }
    //resource deleted once all shared pointers go out of scope.
    return 0;
    }
```

The `weak_ptr` points to resources managed by a `shared_ptr`, but has no say over when those resources go out of scope. Fortunately, it is possible to test whether the weak pointer is still pointing to a valid resource or not. To use the weak pointer, it must be converted to a `shared_ptr`. Here is an example:

```cpp
struct Stuff
    {
    int val;
    Stuff(int a): val(a)
        {std::cout << "Stuff()" << std::endl;}
    ~Stuff()
        {std::cout << "~Stuff()" << std::endl;}
    };

void check(std::weak_ptr<Stuff> weakPtr)
    {
    if(weakPtr.expired())
        std::cout << "Not available" << std::endl;
    else
        std::cout << "Available" << std::endl;
    }

int main()
    {
    std::weak_ptr<Stuff> weakPtr;
        {
        auto sPtr = std::make_shared<Stuff>(50);
        weakPtr = sPtr;
        //weak pointer available here:
        check(weakPtr);
        //output value using weak_ptr:
        auto sPtr2 = weakPtr.lock();
        std::cout << sPtr2->val << std::endl;
        }
    //shared_ptr deleted, so weak ptr not available here:
    check(weakPtr);
    return 0;
    }
```

All in all, these different pointer types take on the burden of managing resource deletion so that you don't have too, while offering different ownership semantics when required. It is well worth becoming comfortable with these.

# _rvalue_ references

In c++, there are two types of expression, _lvalues_ and _rvalues_. _lvalues_ are basically anything that survives beyond a single line; that is, anything with an actual memory address. _lvalues_ can always exist on the left hand side of an expression. _rvalues_ are everything else; expressions whose result would not normally persist longer than that line. _rvalues_ can only ever exist on the right hand side of an expression. Some examples:

```cpp
int number = 10;
int someFunction(){return 4;}
int& anotherFunction(){return number;}

int main()
    {
    int a = 3+5;
    //a is an lvalue. (3+5) is an rvalue.

    int b = someFunction();
    //b is an lvalue, (someFunction()) is an rvalue.

    (a > b? a : b) = 10;
    //the result of (a > b? a : b) is an lvalue.

    anotherFunction() = 3;
    //anotherFunction() is an lvalue:
    //it returns something with an address.

    return 0;
    }
```

Prior to C++11, there was no way to know whether your function was being passed an lvalue or an rvalue. Why does this matter? Well, since rvalues are never again accessed, why not make use of their resources rather than make a potentially expensive copy. Rvalue references are a way of explicitly accessing rvalues.

An rvalue reference is denoted like a regular reference, but uses two `&&` symbols instead of one. It should never be passed with the `const` modifier, as that would defeat the point of being able to use its resources (and thus modify it). Functions that take in rvalue references cannot be passed lvalues, and thus will ordinarily be seen paired up with overloads of them that also accept normal references (also known as _lvalue references_).

One often used example is in adding together resources. Here is how we might write and use an addition function for some random structure without rvalues:

```cpp
struct Stuff
    {
    std::vector<int> v;
    Stuff(int l)
        {
        std::cout << "Constructing" << std::endl;
        for(int i = 0; i < l; i++)
            {
            v.push_back(i);
            }
        }
    Stuff(): Stuff(0)
        {}
    };
Stuff operator+(const Stuff& s1, const Stuff& s2)
    {
    std::cout << "lvalue + lvalue" << std::endl;
    Stuff s3;
    for(auto val : s1.v) s3.v.push_back(val);
    for(auto val : s2.v) s3.v.push_back(val);
    return s3;
    }

int main()
    {
    std::cout << "# Making Stuff objects:" << std::endl;
    Stuff s1(2);
    Stuff s2(5);
    Stuff s3(10);

    std::cout << "# Add them to new Stuff:" << std::endl;
    Stuff s4 = s1 + s2 + s3;

    std::cout << "# Add rvalue Stuffs to new Stuff:" << std::endl;
    Stuff s5 = Stuff(10) + Stuff(20) + Stuff(30);

    std::cout << "# Add mix of Stuffs to new Stuff:" << std::endl;
    Stuff s6 = s1 + Stuff(10) + s2 + Stuff(20);

    return 0;
    }
```

And the output of that is as follows:

```txt
# Making Stuff objects:
Constructing
Constructing
Constructing
# Add them to new Stuff:
lvalue + lvalue
Constructing
lvalue + lvalue
Constructing
# Add rvalue Stuffs to new Stuff:
Constructing
Constructing
Constructing
lvalue + lvalue
Constructing
lvalue + lvalue
Constructing
# Add mix of Stuffs to new Stuff:
Constructing
Constructing
lvalue + lvalue
Constructing
lvalue + lvalue
Constructing
lvalue + lvalue
Constructing
```

In contrast, if we define the following operators to make use of rvalue semantics:

```cpp
//s1 is an rvalue reference:
Stuff operator+(Stuff&& s1, const Stuff& s2)
    {
    std::cout << "rvalue + lvalue" << std::endl;
    for(auto val : s2.v) s1.v.push_back(val);
    return s1;
    }

//s2 is an rvalue reference:
Stuff operator+(const Stuff& s1, Stuff&& s2)
    {
    std::cout << "lvalue + rvalue" << std::endl;

    std::vector<int> tmp(s1.v);
    for(auto val : s2.v) tmp.push_back(val);
    s2.v = std::move(tmp);

    return s2;
    }

//s1 and s2 are rvalue references:
Stuff operator+(Stuff&& s1, Stuff&& s2)
    {
    std::cout << "rvalue + rvalue" << std::endl;
    for(auto val : s1.v) s2.v.push_back(val);
    return s2;
    }
```

The output becomes:

```txt
# Making Stuff objects:
Constructing
Constructing
Constructing
# Add them to new Stuff:
lvalue + lvalue
Constructing
rvalue + lvalue
# Add rvalue Stuffs to new Stuff:
Constructing
Constructing
Constructing
rvalue + rvalue
rvalue + rvalue
# Add mix of Stuffs to new Stuff:
Constructing
Constructing
lvalue + rvalue
rvalue + lvalue
rvalue + rvalue
```

Notice that in the new functions we have added, we avoid creating a new Stuff object entirely (which could be potentially expensive if it was more complex), and instead make use of the already-constructed rvalue that would otherwise be destroyed anyway. In addition, when an rvalue is on the left hand side of the `+` operation, we make use of its existing vector, and just add the additional values onto it, rather than copying both sets of values into a new vector.

## `std::move`

You'll notice that I used std::move in one of the new operators above. All this does is turn an lvalue that is passed into it, into an rvalue. This is useful when you wish to pass something into a function that you never plan on using again anyway, in case it can take advantage of this fact and pillage its resources. Any variable passed somewhere with `std::move` will be put back into a default state; a vector will be emptied for instance.

## `std::forward`

`std::forward` on the other hand works with templates, and converts the argument passed into it back into what it was originally. Consider this example:

```cpp
void someFunc(const int & a)
    {
    std::cout << "lvalue version called" << std::endl;
    }
void someFunc(int && a)
    {
    std::cout << "rvalue version called" << std::endl;
    }

template<class T>
void stuff(T && input)
    {
    //...do stuff...
    //forward argument to new function:
    someFunc(std::forward<T>(input));
    }

int main()
    {
    stuff(5);
    int num = 3;
    stuff(num);
    }
```

The above outputs:

```txt
rvalue version called
lvalue version called
```

Why? Simply put, the template function `stuff` can take in an lvalue or rvalue reference. However, inside the function body, everything is an lvalue again, as it has been named (note that the rvalue passed in can now be used on the left hand side of an expression in the function body). `std::forward` converts the value back into its original form as it was passed in to the function, allowing it to be once again treated as an rvalue **if it was one originally**.

In reality, things are a bit more complex, so I encourage you to read around for more information if you plan on putting this feature to use.

# Variadic templates

Variadic templates are templates that can take a variable number of parameters. They provide a type-safe alternative to regular variadic functions, and enable a range of rather fancy generic programming techniques. I'll kick us off with the simplest example I could think up:

```cpp
template<typename First>
double sum(First value)
    {
    return value;
    }

template<typename First, typename... Rest>
double sum(First value, Rest... others)
    {
    return value + sum(others...);
    }

int main()
    {
    std::cout << sum(2, 3.0, 400, 5) << std::endl;
    }
```

The template `sum` function declared here takes in any number of valid numbers, adds them together, and returns the result in the form of a `double`. The ellipsis (`...`) is used before the variable name in the template declaration to show that this variable represents 0 or more possible inputs. when used after the variable name in the function body, that variable is expanded out into all of the variables it represents. Hence, in the above example, the function body of sum looks like this after the first call:

```cpp
template<typename First, typename... Rest>
double sum(First value, Rest... others)
    {
    return 2 + sum(3.0, 400, 5);
    }
```

As `sum` is called recursively, this expands into:

```cpp
template<typename First, typename... Rest>
double sum(First value, Rest... others)
    {
    return 2 + 3.0 + sum(400, 5);
    }
```

And then:

```cpp
template<typename First, typename... Rest>
double sum(First value, Rest... others)
    {
    return 2 + 3.0 + 400 + sum(5);
    }
```

At which point, we can see why an overload of sum taking in only one value is needed; the packed variable `others` expands into just one variable, which is just returned by the overloaded version of `sum`:

```cpp
template<typename First, typename... Rest>
double sum(First value, Rest... others)
    {
    return 2 + 3.0 + 400 + 5;
    }
```

Now we have expanded all of the templates out, we have the resulting function call that is made upon calling `sum`, which we can see will result in the correct answer. Note that template expansion is done at compile time, essentially creating a separate sum function for each unique set of inputs used with it at compile time.

There is not much that you can do with the packed variables declared using `...` except unpack them to be passed into another function, and get the number of them. This is why typically they are applied recursively, as above.

In this next example, we count the number of arguments passed into a variadic template function using the `sizeof...()` function:

```cpp
template<typename... T>
unsigned int count(T... variables)
    {
    //note that we do not unpack the variables for this:
    return sizeof...(variables);
    }

int main()
    {
    //prints 5:
    std::cout << count(2, 3.0, 400, 5, "hello") << std::endl;
    }
```

One other useful thing that we can do is to apply a function to all of the packed variables to transform them prior to unpacking. Getting back to the sum algorithm, this modification adds one to all of the tail variables each recursion step:

```cpp
template<typename First>
double sum(First value)
    {
    return value;
    }

template<typename First, typename... Rest>
double sum(First value, Rest... others)
    {
    return value + sum((others+1)...);
    }

int main()
    {
    std::cout << sum(1) << std::endl; //1
    std::cout << sum(1, 1) << std::endl; //3
    std::cout << sum(1, 1, 1) << std::endl; //6
    std::cout << sum(1, 1, 1, 1) << std::endl; //10
    std::cout << sum(1, 1, 1, 1, 1) << std::endl; //15
    }
```

## Perfect Forwarding

More usefully, you can wrap variables in functions such as `std::forward`, to preserve rvalue-ness and allow for the perfect forwarding of variables through one function call to another:

```cpp
struct randomStruct
    {
    std::vector<int> v;
    template<typename T>
    randomStruct(T&& a): v(std::forward<T>(a))
        {}
    };

template<typename Struc, typename ...Inputs>
Struc * perfect(Inputs && ...params)
    {
    return new Struc(std::forward<Inputs>(params)...);
    }

int main()
    {
    auto s = perfect<randomStruct>(std::vector<int>{100,200,300,10,40});
    for(auto i : s->v) std::cout << i << " ";
    std::cout << std::endl;
    }
```

In the above, the variadic template function `perfect` takes in any number of arguments as references (optionally rvalue references, given that two &&'s are used), and forwards them to the constructor of whatever is passed in as a template parameter. The `std::forward` function turns any argument that was passed in as an rvalue back into an rvalue again, before sending it off to the constructor. The constructor for `randomStruct` itself forwards the vector taken in to the vector's inbuilt move constructor.

The templating of the `randomStruct` constructor is an alternative to manually writing out two separate declarations for it to take either rvalues or lvalues. the template constructor for `randomStruct` could thus have been replaced with:

```cpp
randomStruct(const std::vector<int> & a): v(a)
  {}
randomStruct(std::vector<int> && a): v(std::forward<std::vector<int>>(a))
  {}
```

which would accomplish the same task. One thing that's neat about variadic templates is that we can delegate the creation of an object to some function, which in turn places the object exactly where it's wanted. This is what the new `emplace` function does on the stl containers:

```cpp
struct Stuff
    {
    int a;
    std::vector<int> b;
    Stuff(int a1, const std::vector<int> & b1): a(a1), b(b1)
        {}
    };

int main()
    {
    std::vector<Stuff> s;
    //pass in arguments to have objects constructed:
    s.emplace_back(10, std::vector<int>{1,2,3,4,5});
    s.emplace_back(8, std::vector<int>{1});
    return 0;
    }
```

# Tuples

A tuple is a type of container that is based on a variadic template, and can take any number of heterogeneous types. One way of looking at it might be as a generic version of a `struct`. Tuples make it easy to return a pack containing multiple values from a function, which can then be unpacked into local variables using the `std::tie` function. Here is a simple example (you'll need the `tuple` header for this):

```cpp
//make a tuple of type std::tuple<std::string,double>
//with contents "Hello" and 4.0:
auto a = std::make_tuple("Hello", 4.0);

std::string word;
double number;

//extract the two variables stored in the tuple into local
//variables "word" and "number":
std::tie(word, number) = a;

//prints "Hello, 4.0":
std::cout << word << ", " << number << std::endl;
```

Tuples can be used similarly to return multiple values from a function. You can also ignore variables stored in the tuple when unpacking them using std::tie, if you don't want to unpack all of them. An example:

```cpp
std::tuple<int, std::string> someFunc()
    {
    int some_number = 12;
    std::string some_string = "Hello";
    return std::make_tuple(some_number, some_string);
    }

int main()
    {
    std::string s;

    //Note: we use std::ignore here to ignore a variable:
    std::tie(std::ignore, s) = someFunc();

    //outputs "Hello":
    std::cout << s << std::endl;
    return 0;
    }
```

Tuples have some handy convenience functions defined for them. You can, for instance, use comparison operators to compare two tuples worth of items. The comparison works through each item one at a time until there is a difference, and then returns that:

```cpp
auto a = std::make_tuple("a", 12, 10);
auto b = std::make_tuple("b", 12, 10);
auto c = std::make_tuple("b", 12, 8);
auto d = std::make_tuple("b", 10, 100);

std::cout << (a < b) << std::endl; //returns 1
std::cout << (b < c) << std::endl; //returns 0
std::cout << (c < d) << std::endl; //returns 0
```

A more efficient way to invoke the comparison of arbitrary values is to use `std::tie`, which when not assigned too, takes in references to arbitrary variables and returns a tuple full of references, as illustrated here:

```cpp
//define some variables:
int n1 = 10, n2 = 20, n3 = 20;
std::string s1("Hello"), s2("Bye"), s3("Woop");

//quick multi-comparison with std::tie:
if(std::tie(n1, s1) < std::tie(n2, s2))
  std::cout << "n1_s1 is less than n2_s2" << std::endl;

//makes a tuple of references to n3 and s3:
auto tup = std::tie(n3, s3);

//change value of n3:
std::get<0>(tup) = 5;

//outputs 5:
std::cout << n3 << std::endl;
```

Above, we see the `std::get` function used, which takes in a template parameter indicating the position of the item to obtain, and takes a tuple as its parameter, returning the variable stored at that position. In this case, that variable is a reference to n3, which subsequently can be modified. using `std::tie` also makes defining custom comparison operators in multi-variable stricts simpler, for example:


```cpp
struct MyStruct
    {
    int a;
    std::string b;
    };

bool operator< (const MyStruct & s1, const MyStruct & s2)
    {
    //one line comparison:
    return std::tie(s1.a, s1.b) < std::tie(s2.a, s2.b);
    }
```

Due to their generic nature, we can also mess around with templates and tuples. The following example uses code taken from [here][stack], and applies a visitor function object to a tuple, which iterates over each item in it, running a function overloaded for the relevant element type:

```cpp
struct visitor
    {
    template<typename T>
    void operator() (T s)
        {
        std::cout << "Unknown: " << s << std::endl;
        }

    void operator() (int s)
        {
        std::cout << "Integer: " << s << std::endl;
        }
    void operator() (double s)
        {
        std::cout << "Double: " << s << std::endl;
        }
    void operator() (const char* s)
        {
        std::cout << "String: " << s << std::endl;
        }
    };

template<std::size_t I = 0, typename Func, typename... Tup>
typename std::enable_if<I == sizeof...(Tup), void>::type
  apply(std::tuple<Tup...> &, Func)
  { }

template<std::size_t I = 0, typename Func, typename... Tup>
typename std::enable_if<I < sizeof...(Tup), void>::type
  apply(std::tuple<Tup...>& t, Func & f)
  {
    f(std::get<I>(t));
    apply<I + 1, Func, Tup...>(t, f);
  }


int main()
    {
    auto t = std::make_tuple(4.0, 3, "hello", false);

    visitor V;
    apply(t, V);

    return 0;
    }
```

output:

```txt
Double: 4
Integer: 3
String: hello
Unknown: 0
```

Essentially, the template functions `apply` take in the tuple, and iterate over each member of it, sending it to the passed in function. The clever bit of code is the `std::enable_if` call. This takes in a boolean as its first template parameter, and the desired return type as the second. If the boolean is false, the `type` member does not exist, causing a substitution failure, and thus ignoring the template. Otherwise, type does exist, and the template function can be used as normal (read up on **SFINAE** for more information).

This functionality enables us to define a stopping case in the form of another template function which becomes valid in place of the original, to stop iterating ocne we have viewed all of the elements. The visitor object then overloads the `operator()` function for different types, and has a template version in place for any unknown types.

Tuples can be handy for the built in functionality that they provide, and can help make some cases more readable and flexible. That said, as values stored inside them are not named unlike in a struct, they may not always be the best choice and may make it less clear as to what your function is returning. However, for swapping variables around quickly, comparing multiple variables with eachother, and some return cases, not to mention their applications in template programming, they seem to be pretty interesting.

# Summary

Well, that about covers it. I didn’t manage to talk about everything, for example the better random number features, timing, or regular expressions (my version of gcc isn't up to scratch yet, and the post was getting rather long anyway), but if you got this far, you'll no doubt have the tools at your disposal to do some pretty cool things now.

Thanks for reading!




[wiki]: http://en.wikipedia.org/wiki/C%2B%2B11
[stack]: http://stackoverflow.com/questions/1198260/iterate-over-tuple
