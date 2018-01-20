+++
title = "Two Handy Tools to Help Build an AJAX Site"
description = "Two utilities I built for a pre-Angular (or Ember) website, which help with routing URL requests to function calls, and storing/monitoring state in a tree structure."
date = "2013-06-10"
+++

This page introduces two lightweight Javascript tools I have created to aid in the process of creating AJAX heavy websites, [Engine.js][engine] for controlling requests for different pages/resources, and [State.js][state] for keeping track of the current state of things clientside, and attaching functions to changes in this state. Both are lightweight and relatively simple, so I'll explain by example:

# Engine.js

_Engine.js_ is used as a clientside routing layer, which acts much like the routing in _Express.js_, and allows functions to be associated with page requests. In addition, functions not defined under a particular route are executed every time, allowing for initialisation and cleanup code surrunding the specific functionality for each request.

The following is a reference cobmined with example usage:

## Reference

### new Engine();

```
//create a new routing engine instance (new is optional):
var routes = new Engine();
```

### engine.add(function1, [function2, function3..]);

Add a function, or list of functions, that fire for any request made. Functions are all passed two arguments, `request` and `next`. The former is an obejct containing useful information regarding the request made, and the latter is a function that must be called at the end of each function provided in order to execute the next function in the chain.

The following example sets up some routes using the `add` function:

```
//called first on every request:
routes.add(function(req, next)
	{
	var output = req.output = [];

	//let's look at some variables we have access to:
	output.push("REQUEST");
	output.push("- requested location: " + req.url);
	output.push("- requested querystring: " + req.querystring);
	output.push("- passed args: " + req.args);

	//look at each individual part of query string:
	for(var parameter in req.query)
		{
		output.push("  o " + parameter + ": " + req.query[parameter]);
		}

	next();
	});

//only called for routes matching /
routes.add("/", function(req, next)
	{
	var output = req.output;

	output.push("HOME PAGE");

	//returns, firing off the next function in the chain:
	return next();
	});

//only called for routes matching /stuff/[somevalue1]/[somevalue2]
routes.add("/stuff/{one}/{two}", function(req, next)
	{
	var output = req.output;

	output.push("STUFF");
	//"one" and "two" become variables in an object:
	output.push("- one: " + req.params.one);
	output.push("- two: " + req.params.two);

	return next();
	});

//called for any route as long as it did not match any of the above
routes.add(/.*/, function(req, next)
	{
	//useful as a catchall route (ie an error 404):
	req.output.push("PAGE NOT FOUND");

	//passing variable to next sets req.error to it:
	next("location "+req.url+" does not exist");
	});

//called last on every request:
routes.add(function(req, next)
	{
	if(req.error) console.log("ERROR: " + req.error);
	else console.log(req.output.join("\n"));

	//we'd need a next() here if there was another function following this.
	});
```

### engine.go(path, [optional arguments]);

Execute any functions valid given the string `path`, passing any additional arguments to these functions as an `args` array stored in the `request` object provided to each function.

Here is the console output for a few example requests, given the routes defined above:

```

routes.go("/");
/*
REQUEST
- requested location: /
- requested querystring:
- passed args:
HOME PAGE
*/

routes.go("/", true, 1, "something");
/*
REQUEST
- requested location: /
- requested querystring:
- passed args: true,1,something
HOME PAGE
*/

routes.go("/?zoo=1&too=2");
/*
REQUEST
- requested location: /
- requested querystring: zoo=1&too=2
  o zoo: 1
  o too: 2
- passed args:
HOME PAGE
*/

routes.go("/randompage/something");
/*
ERROR: location /randompage/something does not exist
*/

routes.go("/stuff/bam/another");
/*
REQUEST
- requested location: /stuff/bam/another
- requested querystring:
- passed args:
STUFF
- one: bam
- two: another
*/
```

## More Information

These examples build a pretty decent picture up of what is going on. The first parameter passed to each routing function is the _request_ object, which contains some useful information (including the location asked for, querystring, and any arguments passed to `routes.go` after the location). The second parameter is the `next` function, and should be called when the routing function has finished, in order to fire off the next routing function. A route can also have a list of functions added for it, to help reusability, such as:

```
routes.add("/somepath", function1, function2, function3);
```

Another thing worth noting, is that anything you add to the _request_ object is present in later function calls as well, so it's handy for passing things through the function chain as I have done with `req.output` in order to build up an output string.

The querystring (anything after the first "?" in the request) is parsed for your convenience, and also added to the request object in both its original and parsed form; it does not influence the routing process at all. In addition, routes themselves can contain variables, as we can see from the use of the `/stuff/{one}/{two}` route, where a variable is any word enclosed in curly braces. These are then accessible in `req.params`, in the above case `req.params.one` and `req.params.two`.

### Automating Things

Once we have defined routes, it's often useful to set the routing engine up to handle changes to the document URL automatically. One major benefit of this is that it preserves browser history. To do this, we just need to set up an interval timer to check for changes to the URL, and pass any changes to the routing engine. Following from the above examples, this could be achieved as follows:

```
(function()
	{
	var old_hash = window.location.hash;
	function check()
		{
		if(window.location.hash !== old_hash)
			{
			old_hash = window.location.hash;
			routes.go(old_hash.replace(/^#!?/, ""));
			}
		}
	var interval = setInterval(check, 100);
	})();

```

This anonymous function just checks the value of the URL hash string (everything including and after the "#" symbol in the current URL), and passes any such changes to the routing engine. Given this, you simply point links in your page to `#[YOUR_ROUTE]` to have the routing functionality kick in and perform whatever is necessary for the given route.

# State.js

_State.js_ is a simple utility for managing the _state_ of variables clientside. Since AJAX heavy pages do no completely reload on each request, maintaining the correct state of the page relies on decent routing functionality (see above for that), as well as ensuring that any state changes that occur are centralized and acted on appropriately.

For example, if user login/logout is handled via AJAX, items in the page such as a display showing the name of the logged in user need to be responsive to the change in this status. In more complex applications, requests may be able to change the value of numerous values on the server, and these values need to be kept in sync with those shown clientside.

_State.js_ is simply an implementation of a storage mechanism for variables, which allows for a hierarchical storage of variables, and allows functions to be attached to any such locations, to ensure that changes to the variables are properly handled.

Thw following is a reference combined with example usage:

## Reference

### new State([start_state]);

```
//create a new instance of State (new prefix is optional):
var s = new State();
```

Optionally, if an argument is passed in to `new State`, it is used as the inital state.

### state.setValue(path, value);

Sets the value at the string `path` to the value `value`, where `path` is a period separated location.

```
//set a basic path to a value:
s.setValue("hello", 200);

//set a compound path to a value:
s.setValue("list.one", "A");
s.setValue("list.two", "B");

//set a path to a number of values:
s.setValue("another_list", {three: "C", four: "D"});

//set every value simultaneously:
s.setValue({
	hello: 200,
	list: {
		one: "A",
		two: "B"
		},
	another_list: {
		three: "C",
		four: "D"
		}
	});
```

Worth noting here is that objects and paths are interchangeable; paths are internally converted into objects to allow for the above.

### state.getValue(path)

Gets a **copy** of the value stored at the string `path`.

```
//carrying on from the examples above.
s.getValue("list"); //returns object {one: "A", two: "B"}
s.getValue(".list"); //returns object {one: "A", two: "B"}
s.getValue("list.one"); //returns "A"
s.getValue(".list.one"); //returns "A"
s.getValue("something.blah"); //any unset path returns undefined.

//returns object containing everything:
s.getValue();
s.getValue("");
s.getValue(".");
```

### state.setFunction(path, func);

Sets a function `func` to execute whenever the value stored at the string `path` changes. `func` received three arguments, `old_value` (the previous value at that path), `new_value` (the new value at that path) and a string equal to `path`.

If path is an object, sets any function stored within the object to fire when the path equal to that functions location in the object is changed.

```
var f = function(old_val, new_val, path)
	{
	console.log(path +": "+old_val+" -> "+new_val);
	}

//basic function setting:
s.setFunction("hello", f);

//set function to fire when anything under "list" changes:
s.setFunction("list", function(old_val, new_val, path)
	{
	//log values at "list" when any of them change:
	if(typeof new_val == "object") for(var i in new_val)
		{
		console.log(i + ": " + new_val[i]);
		}
	else console.log(new_val);
	});

//sets a bunch of paths to the function f:
s.setFunction({ another_list: {three: f, four: f, five: f}, random: f});

//set two functions under a given path to function f:
s.setFunction("list", {one: f, two: f});
```

### state.removeFunction(path, [path2]);

Removes the function linked to the path `path`. If `path` is an object, all functions at paths present in the object are removed. If `path2` is present and is an object, all functions at paths in `path2` prefixed with path are removed.

```
//remove function at "list":
s.removeFunction("list");

//remove functions at "list" and "hello":
s.removeFunction({list:1, hello:1});

//remove functions "list.one", "list.two", "list.another.three":
s.removeFunction("list", {one:1, two:2, another:{three:1} });
```

### state.triggerFunction(path);

Executes the function linked to the path `path`. The function is passed the value at that path twice (in place of the old and new values sent to it ordinarily) as well as the path it is called from.

```
//trigger the function held at "list.one":
s.triggerFunction("list.one");
```

## More Information

Functions triggered either with `triggerFunction` or by a change in the state are each passed a copy of the old and new value at that path. This means that you can reassign/reuse the parameters passed in to functions freely, without having to worry about where else they may be referenced. This comes at a small overhead cost equating to needing to copy any values found to have changed if there exists a function or functions triggered by that change.

As such, maintaining a very large set of values in the state, and making frequent changes to them, will result in a significant amount of copying of variables. I have deemed this worthwhile however in order to make the entire system more robust, and give the end user less to worry about.

# Conclusion

I have introduced two utilities developed to help manage my AJAX heavy website, and modified for public consumption. If you have plans to use a lot of AJAX in your site, I encourage you to take a look and see if they can save you some time! Download links:

- [Engine.js][engine] for route management.
- [State.js][state] for variable management.

If you find any issues, or otherwise have any suggestions or comments, give me a shout!


[engine]: jw.engine.js
[state]: jw.state.js












