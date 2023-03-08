+++
title = "C++ Optimization: Making use of Memory Pools"
description = "Utilising memory pools can be one of the most significant optimisations you can make to a C++ program. Here, I walk through building and using one from the ground up."
date = 2013-05-03
[extra]
created = "2013-05-02"
+++

This post will explore the creating of a flexible memory pool in C++, and the performance benefits of using it.

# Introduction

Allocating and deallocating memory are expensive operations. In cases where you are creating and destroying a lot of instances of some object, you will be doing a lot of memory allocation and deallocation. Particularly in the case of very small objects, the cost of finding a chunk of memory to allocate for them is somewhat disproportionate to the cost of initializing the object to some desired state.

A number of common scenarios are allocation heavy. For instance, in computer games you may have object representing bullets fired, which are constantly being created and destroyed as the bullets go into and out of existence. Another example may be in linked list structures, where items are frequently being inserted and removed.

Wouldn't it be nice then if we could reuse the memory that we allocate for these objects, rather than spending our time freeing memory up, only to assign new chunks of memory soon after. By doing so, we can significantly reduce these expensive costs. This is where memory pools come in.

Memory pools take control of the creating and destroying of objects. In the simplest case, rather than freeing up memory every time an object is destroyed, we hold onto its memory location - known as adding it to the pool - knowing we'll be needing it again. Next time an object wants to be created, we look to see if there is any free memory available in the pool. If there is, we use that, saving the cost of looking for and allocating a new chunk of memory for it. If not, we simply allocate it normally.

A slightly more complex approach has us allocate large blocks of memory, from which we can slice bits off for our objects as we need them. This can also be combined with the above approach, to use and reuse the same chunks multiple times.

# Some Prerequisite Knowledge

C++ has one particularly useful feature to help pull this off; placement new. This is used similarly to how the `new` operator is used to create new instances of some object dynamically, except that we provide it with a memory location to do the constructing in, rather than allowing it to allocate a suitable chunk of memory. Here it is in action:

```cpp

struct SomeObject
	{
	int val;
	SomeObject(int val_in): val(val_in) {}
	};

int main()
	{

	//use the global operator new to allocate some bytes memory:
	void * free_space = ::operator new(sizeof(SomeObject));

	//use placement new, to stick an instance of SomeObject there:
	SomeObject * o = new (free_space) SomeObject(100);

	//prints 100:
	std::cout << o->val << std::endl;

	//call the destructor on the object:
	o->~SomeObject();

	//delete the memory created for it:
   ::operator delete(free_space);

	return 0;
	}
```

In the above, we also see the global `operator new` and `operator delete` functions used. The former simply allocates some number of bytes of memory (much like `malloc` in _C_), and returns a pointer to them. The latter frees up the memory at the address provided. In our case, we want to allocate enough memory to fit an instance of `SomeObject`, so we use `sizeof` to return the number of bytes required to hold it, and allocate that number of bytes in memory.

Next, we use the placement new operator to construct an instance of our object at a given location in memory. We pass it our `free_space` pointer, which points to the memory location we wish to construct it at. The syntax of placement new, as seen above, is simply:

```cpp
new (memory_location) ClassName(constructor_arguments);
```

When we are done with the object, we perform the reverse of the above, by first explicitly calling the object destructor (which quickly becomes necessary in anything much more complex than the above), and then using the global operator delete to free up the memory again.

# Making our First Object Pool

Given the above, we can start to put together our first object pool. Let's get straight to the code:

```cpp
template<typename Type>
class BasicPool
    {
    public:
    //constructor (we don't need to do anything here)
    BasicPool() {}

    //delete the copy constructor; we can't copy it:
    BasicPool(const BasicPool &)=delete;

    //move constructor, so we can move it:
    BasicPool(BasicPool && other)
        {
        this->free(std::move(other.free));
        }

    //allocate a chunk of memory as big as Type needs:
    void * allocate()
        {
        void * place;
        if(!free.empty())
            {
            place = static_cast<void*>(free.top());
            free.pop();
            }
        else
            {
            place = ::operator new(sizeof(Type));
            }
        return place;
        }

    //mark some memory as available (no longer used):
    void deallocate(void * o)
        {
        free.push(static_cast<Type*>(o));
        }

    //delete all of the available memory chunks:
    ~BasicPool()
        {
        while(!free.empty())
            {
            ::operator delete(free.top());
            free.pop();
            }
        }

    private:

    //stack to hold pointers to free chunks:
    std::stack<Type*> free;
    };
```

This is a template class, enabling us to create a pool for any object type that we want. It could be used as follows:

```cpp
struct SomeObject
    {
    int val;
    SomeObject(int val_in): val(val_in) {}
    };

int main()
    {
    BasicPool<SomeObject> p;

    //create an instance of SomeObject from our pool:
    void * free_space = p.allocate();
    SomeObject * o = new (free_space) SomeObject(10);

    //destroy it:
    p.deallocate(o);
    o->~SomeObject();

    return 0;
    }
```

The main functions then are just `allocate` and `deallocate`. The former outputs a pointer to an available chunk of memory. It does so by checking whether there is any free memory pointed to in the stack named `free`. If there is not, it creates a new chunk of the required size. The `deallocate` function simply adds the memory at the given location back into the pool, so that it can be handed out next time an object of type `SomeObject` needs memory. The memory location passed to `deallocate` must be of the same size as those handed out by `allocate`, otherwise we'll end up with issues.

It would be nice if we could simplify the interface to this a little, and remove this potential issue. Thankfully, with C++11 variadic templates (read my previous post [here][cpp11] for more about those) we can. We add two functions to our memory pool, create and remove, that hide the mess away for us:

```cpp
template<typename Type>
class BasicPool
    {
    public:
    BasicPool() {}

    //delete the copy constructor; we can't copy it:
    BasicPool(const BasicPool &)=delete;

    //move constructor; we can move it:
    BasicPool(BasicPool && other)
        {
        this->free(std::move(other.free));
        }

    //#### create an instance of Type:
    template<typename... Args>
    Type * create(Args && ...args)
        {
        Type * place = (Type*)(allocate());
        try{ new (place) Type(std::forward<Args>(args)...); }
        catch(...) { free.push(place); throw; }
        return place;
        }

    //#### remove an instance of Type (add memory to the pool):
    void remove(Type * o)
        {
        o->~Type();
        free.push(o);
        }

    //allocate a chunk of memory as big as Type needs:
    void * allocate()
        {
        void * place;
        if(!free.empty())
            {
            place = static_cast<void*>(free.top());
            free.pop();
            }
        else
            {
            place = operator new(sizeof(Type));
            }
        return place;
        }

    //mark some memory as available (no longer used):
    void deallocate(void * o)
        {
        free.push(static_cast<Type*>(o));
        }

    //delete all of the available memory chunks:
    ~BasicPool()
        {
        while(!free.empty())
            {
            ::operator delete(free.top());
            free.pop();
            }
        }

    private:

    //stack to hold pointers to free chunks:
    std::stack<Type*> free;
    };
```

What's going on here then? Well, the `create` function takes in a variable number of arguments (whatever we would normally pass to the constructor). It then uses the `allocate` function as before to provide a chunk of memory to use, but in addition, it forwards the arguments passed in to the constructor of whatever `Type` is, and constructs an instance of the object using _placement new_. It then returns a pointer of `Type` which points to that instance.

It is important to note the try/catch block surrounding the placement new; if for some reason our object type throws an error on construction, we want to free up that memory again to avoid potential leaks.

The `remove` function simply adds a pointer to the location in memory that is passed in to the `free` stack, just like `deallocate`, but in addition calls the destructor of the object to do any cleaning up that it requires.

Now, we can very easily create and destroy objects, like this:

```cpp
int main()
    {
    BasicPool<SomeObject> p;

    //create an instance of SomeObject from the pool:
    SomeObject * o = p.create(10);

    //destroy it when done, putting memory back into the pool:
    p.remove(o);

    return 0;
    }
```

Nice and simple! You'll probably also want to add a function to deallocate all of the free memory currently held in the pool, but I'll leave that as an exercise for you.

# A Comparison

So, how exactly does using the simple object pool we created above compare with just using the `new` and `delete` operators to handle our memory management? Well, that obviously depends on how you use it; the more that objects are deleted and later created again - in other words, the more allocation and deallocation we can save by using a pool - the more benefit you'll see. Here is a simple test set-up, where we do a bunch of creating and deleting of our very simple object:

```cpp
void usePool()
    {
    std::stack<SomeObject*> s;
    BasicPool<SomeObject> p;
    for(unsigned i = 0; i < 100; i++)
        {
        for(unsigned j = 0; j < 1000; j++)
            {
            s.push(p.create(1));
            }
        while(!s.empty())
            {
            p.remove(s.top());
            s.pop();
            }
        }
    }

void noPool()
    {
    std::stack<SomeObject*> s;
    for(unsigned i = 0; i < 100; i++)
        {
        for(unsigned j = 0; j < 1000; j++)
            {
            s.push(new SomeObject(1));
            }
        while(!s.empty())
            {
            delete s.top();
            s.pop();
            }
        }
    }

int main()
    {
    usePool();
    noPool();
    return 0;
    }
```

All this test does is push a bunch of pointers to dynamically created instances of `SomeObject` - our structure holding a single integer - to a stack, and then delete them from that stack. The `usePool` function does this using our memory pool, and the `noPool` function does this using `new` and `delete`. Using Valgrind, the number of instructions for each can be counted (fewer is better). The result:

```txt
usePool(): 7,925,308
noPool(): 25,636,536
```

So, the `usePool` function look less than a third the amount of time to execute! This highlights exactly how much time is spent allocating and deallocating memory, and consequently how much time can be saved by reducing it. The pool version is faster even when the outer loop is executed only twice (~12%), saving just 1000 `int` sized deallocations and allocations. Increasing the number of variables held in SomeObject from 1 to 5 also has very little impact, but favours using the memory pool slightly more.

Running this comparison without the overhead of a stack of pointers can result in a far more noticeable difference (I found a 10x speed up in other tests comparing the object pool above with `new` and `delete`).

# Further Improvements

Of course, we can try to improve on this further by preallocating memory in larger blocks, rather than per instance, and then assigning parts of the block to instances of our desired type. However, this does come with the drawback of being harder to free up the blocks of memory once they have been created, as it is harder to keep track of what memory within a given block is still in use at any point in time.

In my experience, the simple version above cut somewhere about 30% off the execution time of my C++ Sequitur implementation, which does quite a lot of symbol creating/destroying over the course of its lifetime. Using a block allocation variant resulted in a further speedup of another 9%, but on balance was not worth the difficulty in freeing up the potentially significant amount of memory used.

## Inheriting Memory Pool Functionality

Another thing that we can do to make our object pool almost entirely transparent, is to override the member `operator new` and `operator delete` functions for a class to use it. Like the global versions, these handle allocating and deleting the memory only, not constructing or destructing the instance. As such, the `allocate` and `deallocate` functions of our object pool come in handy.

I did this myself by writing a small template class, which could be inherited from to give the inheriting class this transparent functionality; it looks something like the following:

```cpp
template<typename Child, template<typename=Child> class PoolClass = BasicPool >
class InheritablePool
  {
  public:
  //provide access from the inheriting object to clear the pool:
  static void clearPool()
      {
      pool.clear();
      }

  static void * operator new(size_t)
      {
      return pool.allocate();
      }
  static void operator delete(void* in)
      {
      pool.deallocate(in);
      }

  private:
  //protected ctors so only Child can make instances:
  InheritablePool() {}
  InheritablePool(const InheritablePool &) {}

  //our object pool (where PoolClass defaults to BasicPool):
  static PoolClass<Child> pool;

  //Child class can make instances:
  friend Child;
  };

//define the static pool variable:
template<typename Child, template<typename> class PoolClass>
PoolClass<Child> InheritablePool<Child,PoolClass>::pool;

//make inheriting a little less wordy using a define:
#define UseObjectPool( x ) public virtual InheritablePool<x>
```

This can then be used as follows, using the example of our `SomeObject` class:

```cpp
struct SomeObject: UseObjectPool(SomeObject)
    {
    int val;
    SomeObject(int val_in): val(val_in) {}
    };
```

Resulting in the benefits of using a memory pool, combined with the transparency of using the standard syntax. The function `SomeObject::clearPool()` would free up any memory held onto by the object pool, if it became necessary to do so (for example, we had deleted all instances of our object and wanted our memory back!).

A downside to this approach is that the class should not be inherited from by anything that adds new member variables. In the above case, the use of `friend Child;` combined with the private constructor and copy constructor, and the virtual inheritance (which is simplified by the `#define`), prevents any instantiations of classes that inherit from anything using the `InheritablePool` class. As such, the `SomeObject` class, as defined above, can not be inherited from.

Whether or not you would choose to block the inheritance in this way, or indeed make use of object pools like this, is up to you. In my case, I would probably have preferred to use them explicitly, but the modifications to existing code would have taken significantly more effort and coupled things together more than I would have liked, whereas the class inheritance solution above allows object pools to very easily be made use of by existing classes.

If you do inherit object pool functionality as above, make sure to free up any available memory left in the pool when necessary. In the above case, the line:

```cpp
SomeObject::clearPool();
```

Would do just that.

# Final Remarks

Anyway, This has been a relatively brief but hopefully useful tutorial on C++ memory pools. Writing the code is relatively easy, and integrating them into existing code using features such as inheritance is relatively easy to do. The performance gains made are substantial, making them invaluable in high performance code.

Feel free to make use of any of the code above (you will need C++11 support), and get in touch if you have any queries or comments!

[cpp11]: ./posts/cpp11-new-additions/index.md


