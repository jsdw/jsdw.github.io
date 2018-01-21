+++
title = "AngularJS: Demystifying Services and Dependency Injection"
description = "This post is my attempt to unravel the mystery often surrounding the difference between services, providers, values and the like in Angular. I then run through how dependency injection works, building a plain Javascript example along the way."
date = "2014-02-23"
+++

For the last few months, I have been working with AngularJS to build web applications. This website itself is a simple example of that, but at work we are building far more complex single-page applications, and it is there that Angular really starts to shine.

In this post, I want to demystify _services_ in Angular, and show how they relate to providers, controllers, filters and directives. I'll then go on to talk about _Dependency Injection_ - the means by which services (and providers) are passed about in Angular, running through how you might implement the same functionality yourself and make use of that provided in Angular.

Let's get started!

# Services in Angular

A service is the main currency in angular; it is something that can be configured, created, and then handed to (almost) any function that supports _Dependency Injection_, which I'll get onto. The methods to create services are exposed on the `$provide` service provider, and also made available on the _module_ object created when you make a new app, for convenience.

There are various different ways to create a service, but the end result is basically the same; a constructor function is registered with the application _injector_. When the service is asked for, this constructor function is called in order to create a service instance. That same instance is then given to anything that has asked for it.

Here are some different ways to create the same service, working from simplest but least flexible to most complete:

```
//first, we create a new angular module:
var App = angular.module("myApp", []);

//the below all create a service called myService that
//returns the object { hello: "hi" }

//declare is as a value; can't inject anything in
//this way, and cant configure via a provider,
//but very concise:
App.value("myService", {
	hello: "hi"
});

//As above, but can't be intercepted by a decorator
//either:
App.constant("myService", {
	hello: "hi"
})

//provide a function that can have things injected
//into, which returns the result of calling it
//with 'new':
App.service("myService", function(){

	this.hello = "hi";

});

//as above, but the service instance is whatever
//the function provided returns:
App.factory("myService", function(){

	return {
		hello: "hi"
	};

});

//create a provider, which allows pre-run
//configuration of the service, and returns
//the above factory function as this.$get:
App.provider("myService", function(){

	this.$get = function(){
		return { hello: "hi" };
	}

});

//any of the above can also be done using
//the $provide provider directly, rather
//than the above shortcuts:
App.config(function($provide){

	$provide.service("myService", function(){
		this.hello = "hi";
	})

	$provide.factory("myService", function(){
		return { hello: "hi" };
	});

	//... and so on.

});

//Now, we can use the service somewhere, for example:

App.controller("main", function(myService){

	//set var hi to "hi":
	var hi = myService.hello;

});

App.run(function(myService, $rootScope){

	//set a value on the rootscope:
	$rootScope.hi = myService.hello;

});

```

Some notes on the above:

- `App.value` and `App.constant` are shortcut methods for directly assigning a value to an instance of `myService`. The former can be _decorated_, while the latter can't. More on decorators later!
- `App.service` and `App.factory` both ask for functions (that other services can be injected into). The former is called with _new_ to instantiate the service, whereas the latter returns the service.
- `App.provider` is the most complete method. It defines a service provider, which is itself instantiated with _new_, and then expected to return a function the same as `App.factory` is passed as `this.$get`; the result of which instantiates the service itself.

In each case, once a service is instantiated, that same instance is passed to everything that asks for it. In other words, services are _singletons_. Only one instance of each service ever exists in your app (well, strictly speaking, only one instance exists per _injector_).

The advantage of using the `provider` method seen above is that the provider can expose methods which can be used to configure the service before it is instantiated. The configuration phase of an application occurs before it kicks off, and thus provides an opportunity to configure services to suit our needs before they are created.

Here is a simple example:

```
var App = angular.module("myApp", []);

//create a provider, which will
//provide us a service called myService:
App.provider("myService", function(){

	var back = { output: "hi" };

	//expose a method on this provider
	//which alters what will be returned
	//when this service is asked for:
	this.setOutput = function(val){
		back.output = val;
	}

	//this is the function that returns the
	//service instance that is passed around.
	//in this case, the service instance is
	//just the "back" object:
	this.$get = function(){ return back; };
});

//given the above, we can configure myService
//before the App starts. Only providers
//can be passed in at this stage, as no
//services have been instantiated yet.
//
//notice that we append the word Provider
//to our service name to get the services
//provider:
App.config(function(myServiceProvider){
	myServiceProvider.setOutput("bye");
});

//now, when we run the app, the service will
//have a different value:
App.run(function(myService){
	console.log(myService.output);
	//outputs "bye".
});

```

In this example, a provider for `myService` is created, which is then configured in the Angular `config` phase before the app kicks off. This configuration stage allows you to create more flexible, reusable services which can be configured according to the needs of the application before runtime. Notice that at config time, service instances have not been created, and so only providers can be passed in. At run time, the tables turn and only services can be injected (values and constants are themselves just services too).

## Where do Controllers, Filters and Directives fit in?

Controllers, filters and directives are not services themselves, and so can't be injected into things. They are actually created from functions that are registered to the relevant service providers before the app kicks off (`$controllerProvider`, `$filterProvider` and `$compileProvider`). As an example, one can create a controller one of these two ways:

```
var App = angular.module("myApp", []);

//1. construct a controller using the provided shortcut
//   function:
App.controller("myController", function($scope){
	/* controller logic here */
});

//2. construct a controller by registering its conatructor with the
//   controllerProvider at app config time:
App.config(function($controllerProvider){

	$controllerProvider.register("myController", function($scope){
		/* controller logic here */
	});

});

```

The `App.controller` function is just a useful shortcut for registering a controller with the relevant provider. The same is true of filter functions and directives. Angular provides shortcut methods as these are constructed fairly often, but the end result is the same.

## Decorating Services

Services in angular can be intercepted before the app kicks off, and modified to better suit your needs. This is useful if you're using third party code for instance, and want to augment it without touching their code.

As well as providing functions to create services, the $provide service provider also exposes a method to decorate services, but as this is less commonly used, a shortcut to it is not exposed on the angular object itself:

```
var App = angular.module("myApp", []);

//create a very simple service (or assume
//that it has been created by a third party):
App.factory("myService", function(){
	return { hello: "hi!" };
});

//decorate it to suit your needs during the app config
//phase:
App.config(function($provide){

	$provide.decorator("myService", function($delegate){

		//$delegate is a special local which is an instance
		//of the original service:
		$delegate.bye = "bye!";

		//when someone asks for the service myService,
		//they now get whatever you return here. You can
		//return whatever you want, even something entirely
		//different:
		return $delegate;

	});

});

//use the service somewhere:
App.controller("myCtrl", function(myService){

	console.log(myService.hello);
	//outputs: "hi!"

	console.log(myService.bye);
	//outputs: "bye!"

});

```

This is a very simple example of a very powerful feature. Not only can services be configured before runtime during the application config phase if they have been created with a configurable service provider, but they can be modified or simply replaced entirely in the config phase as a result of decoration.

# Dependency Injection

Thus far, you have seen many examples of dependency injection. It is the process that occurs every time you ask for an instance of a service in a function which is going to be invoked by angular (which includes those used to create directives, controllers, and other services). Here's an example involving injecting a service and scope object in to a controller constructor function:

```
//create a new Angular app:
var App = angular.module("myApp", ["ng"]);

//register a controller on it. The $scope and $interval
//services are "injected" into the controller:
App.controller("test", function($scope, $interval){

	//do things with $scope and $interval..
	$scope.counter = 0;
	$interval(function(){
		$scope.counter++;
	}, 1000)

});

//Despite the parameter names being reversed, this
//declaration is equivalent to the above:
App.controller("test", function($interval, $scope){

	$scope.counter = 0;
	$interval(function(){
		$scope.counter++;
	}, 1000)

});
```
In the above controller declarations, parameters are being passed in by name rather than by argument position. This is the magic of Dependency Injection (DI) in Angular, and it makes it possible to ask for service instances by name when you define a function in any place that Angular supports DI. But how does Angular do this?

The answer lies in the `toString()` method. Applied to a function, we get a printout of it as a string, like so:

```
//declare a simple function:
function hello(one, two){
	console.log("passed in:", one, two);
}

//print it in string form:
console.log(hello.toString());

//outputs:
// 'function hello(one, two){ console.log("passed in:", one, two); }'
```

With this, it becomes quite easy all of a sudden to extract the variable names out of a given function with a regular expression:


```
//print an array of variable names passed to a provided function:
function annotate(fn){

	//use a RegExp to match the arguments passed in:
	var strArgs = fn.toString().match(/function [^(]*\(([^)]*)\)/)[1];

	//if no args passed, return an empty array:
	if(!strArgs) return [];

	//else, split the args string into an array and return that:
	return strArgs.split(/\s*,\s*/);
}

//example usage:
annotate(function hello(one ,two){}); //returns: ["one", "two"]

```

Given that we know the names of variables passed in to a function, we can produce a wrapper which takes a function, context and object containing values to pass in, and have the correct values provided to the function depending on its parameters:

```
//invokes a function fn, with an args object to pass in values:
function invoke(fn, context, args){

	//use the annotate function above to get the param names:
	var fnParams = annotate(fn);

	//convert the param names to values from the args object:
	var fnArgs = fnParams.map(function(param){
		return args[param];
	});

	//call the function where params are now provided by the "args" object:
	return fn.apply(context, fnArgs);
}

//example usage:
function invokeMe(one, two){ console.log("back:",one,two); }

invoke(invokeMe, null, { one: 1, two: 2 });
//outputs: "back: 1 2"

invoke(invokeMe, null, { two: 100, one: 40 });
//outputs: "back: 40 100"

```

All of a sudden, we have a function that can invoke another function with whatever arguments we supply from an object, mapping function parameter names to object keys. The order that parameters are passed in is no longer relevant; instead the names of the parameters are.

In many places within Angular, functions are _invoked_ in much the same way, although in Angular you have a few options for specifying the list of services you'd like passed in to an invokable function besides just using function parameter names:

```
//these are all the same:

//1. get parameter names from function params as above:
function(one, two){ /*...*/ }

//2. get parameter names from a property on the function.
//   params a and b become the result of getting services
//   called 'one' and 'two':
function invokable(a,b){ /*...*/ }
invokable.$inject = ["one", "two"];

//3. an inline version of the above:
var invokableFunc = ["one", "two", function(a, b){ /*...*/ }];

```

The first method is obviously most concise, but Javascript minification can screw with variable names. As such, 2 and 3 are more robust solutions, and ensure that your code will always work as it should, even when minified.

By applying the above, we can define something like a controller in angular any one of these three ways:

```
// 1.
App.controller("test", function($interval, $scope){
	/*...*/
});

// 2.
function testCtrl($interval, $scope){ /*...*/ }
testCtrl.$inject = ["$interval", "$scope"];
App.controller("test", testCtrl);

//3.
App.controller("test", [
	"$interval",
	"$scope",
function($interval, $scope){
	/*...*/
}])

```

Internally, angular uses a function called `invoke` defined on the injector to do this. If an array is provided in place of a function, it knows that we are trying to use method 3. Otherwise, it looks to see if the function has an `$inject` property attached to it, and if so can use method 2. Finally, it will fall back to using method 1.

One of the great things about Angular is that it exposes the bits you need to pull this off yourself. The `$inject` service in angular is an instance of the injector being used in the application, so you can point it at your own functions to harness the power of service injection for yourself.

Below, we use `invoke` on a custom function to get hold of angular services inside of it:

```
var App = angular.module("myApp", []);

App.run(function($injector){

	//a custom function that we want to inject
	//angular goodness into:
	var fn = function($interval){
		var count = 0;
		$interval(
			function(){ console.log(count); count++ },
			1000
		);
	}

	//invoke the custom function, pulling in the
	//relevant angular service(s):
	$injector.invoke(fn);

	//..causes an incrementing counter to fire every 1000ms
});

```

Taken one step further however, we can pass in an object whose keys are available to be used as parameters in the function being invoked, much like the custom `invoke` function we created ourselves above:

```
var App = angular.module("myApp", []);

App.run(function($injector){

	//a custom function to inject custom things into
	//as well as angular services:
	function fn(a, b, to){
		to(
			function(){
				console.log("back:", a, b);
			},
			2000
		);
	}

	//all methods of annotating functions are supported.
	//The below requests one, two and timeout as the
	//three args of fn:
	fn.$inject = ["one", "two", "$timeout"];

	//invoke the custom function, supplying 'locals'
	//as a third parameter. These override any services
	//that might exist with the same name, and are provided
	//should the function ask for them:
	$injector.invoke(fn, null, {one: 1, two: 2});

	//..logs: "back 1 2"
});
```

Using the `annotate` function, we can shape this injection process to work however we wish, by finding out what things a function wants injected into itself and then working with that information to provide it whatever we like.

The below example injects values only from our predefined "pool", and sets anything else asked for to `null`:

```
var App = angular.module("myApp", []);

App.run(function($injector){

	//custom arg pool to use:
	var pool = {
		one: 1,
		two: 2
	};

	//a function requesting args one,two,three:
	function fn(one, two, three){
		console.log(one, two, three);
	}

	//find out what args the function wants:
	var argsArr = $injector.annotate(fn);

	//build locals to pass to function, getting their values
	//from the pool or null otherwise:
	var locals = {};
	strArgs.forEach(function(arg){
		if(pool[arg]) locals[arg] = pool[arg];
		else locals[arg] = null;
	});

	//pass them in:
	$injector.invoke(fn, null, locals);

	//logs: 1 2 null

});
```

Pretty neat, huh? By using `invoke`, one can make any function injectable just like many native angular ones. This is particularly useful if you expose functionality on a service provider which requires callback functions; the callbacks can be passed custom variables that may be of relevance, as well as other angular services to make use of.

Anyway, that's enough for one post! I hope that things are a little clearer now. Stay tuned for more on Angular!
