+++
title = "Angular and React: A Tale of Two Frameworks"
description = "Having used Angular pretty heavily for over a year at work and more recently ReactJS from Facebook in rebuilding my website, I thought I'd share my views on how they compare."
date = 2015-03-15
+++

# TL;DR

Angular allows you to create more beautiful interfaces and in fewer lines of code, but at the cost of a much steeper learning curve and many more concepts to master before this is possible. The separation of presentation and logic is nice and makes collaboration easier. React is less fully featured and more verbose but it's also clear and easy to understand in comparison, and feels like the more elegant of the two with lots of potential going forward.

> If you want to see all of the following examples in action, I have made them available [here][example-code]. You should also be able to paste this code straight into [plunkr][plunkr] to play with interactively.

# Introduction

## Angular in a Nutshell

Angular is a relatively popular framework from Google that's all about managing updates to your HTML in response to changes in your data so you don't have to. It gives you a bunch of goodies out of the box, including:

- A Dependency Injection core which can help with isolating and testing components.
- A way to create your own dependencies to inject where needed - known as services - things that encapsulate chunks of useful functionality (for example code to fetch data from your backend, store state, perform generic operations on data etc).
- A way to interface your handy services and data with HTML - known as controllers - allowing you to present your data and make it possible to interact with it.
- A way to create components - known as directives - that wrap this combination of controller and HTML together and allows for the creation of reusable widgets (for example comment forms, fancy sliders, dropdowns etc).

Angular comes with a bunch of useful services built in such as `$http` which allows you to request data from a backend, `$q` to make use of promises, page routing to handle changes to the URL, and many others.

Its primary goal is to keep the HTML in sync with your data, so that you only have to worry about changes to your data and the HTML updates as required in response to them.

Angular also strives to make testing your code as approachable as possible; everything in angular declares a list of dependencies explicitly in order to make use of those things, and so by mocking those dependencies with fake implementations we can test the thing itself to make sure it acts as expected.

It also aims to give you everything you need out of the box to throw together some pretty cool things with minimal fuss. For anything more complicated, or a larger project where well organized components becomes much more important, you will end up resorting to creating your own directives, which is where the initial learning curve begins to look a little more like a cliff face for many.

## React in a Nutshell

React has a lot in common with Angular. It's created by another big player in the web game - Facebook - and its primary aim is also managing updates to your HTML in response to your data. React doesn't give you as much out of the box as Angular, instead focusing on creating reusable components. It can most easily be compared with Angular directives.

Angular allows you to extend the vocabulary of HTML, adding custom elements and attributes to implement new functionality while keeping the more complex logic separate in Javascript land. React on the other hand couples view and logic in the Javascript. To make things prettier it provides a JSX syntax which allows you to write what looks much like XML in your Javascript, which is compiled down to React function calls representing the desired HTML structure. React veers away from using actual heavyweight DOM elements in favour of its own stripped down representation.

React also has some goodies if you use _Node.js_ on the backend; it can provide pre-rendered components on page load which the Javascript on the frontend then takes control of. This is made possible by its lack of reliance on real DOM elements, and means that pages can display properly before Javascript kicks in - useful for allowing search bots to scrape your pages without fuss. Testing in React is also facilitated by this ability to render components to strings, though this does not take you as far as the dependency injection route can.

React keeps things up to date by generating its stripped down version of DOM elements each time an update might have happened, and comparing it with the current version. It can then use that to work out where to splice changes into the HTML.

React, in comparison to angular, is very easy to learn, having fewer moving parts than angular and a simple, well defined approach to creating components. Less built in magic does however mean that code tends to be more verbose. You also don't get the framework around components that angular gives you, although (especially for smaller projects) it is often pretty straightforward to create the missing parts yourself.

# Compare and Contrast Time

Let's look at how we'd do some things in both frameworks, and how they would handle it. This will focus generally on slightly more complex examples, since often that is where you hit a stumbling block with these frameworks. I'll try to give at least a general idea of what's going on, but won't go into depth. Hopefully you'll finish with a general feel for how easy or difficult it is to get real things done in each framework.

Let's start with communication; how to pass data back and forward between your different components.

## Communication Between Components: Vertical

This is one of the first things you'll end up doing in either frameworks. Let's get straight into an example in Angular.

Our very simple HTML fragment looks like this:

```
<div ng-app="myApp">
	<div ng-controller="details as ctrl">
		{{ctrl.numbers | json}}
		<div inner name="ctrl.name" numbers="ctrl.numbers"></div>
	</div>
</div>
```

We declare that we'll be using the `details` controller (aliased as `ctrl`), which exposes `numbers`, an array, and `name`, a string. We render the array of numbers as a JSON string by passing it through the built in Angular `json` filter. We also pass these into a custom component (or "directive" as it's known in Angular), which then does what it likes with them.

Our inner component displays the numbers in a different format to show how we might make use of data passed in ,but also modifies it itself to demonstrate two way binding. The outer controller also modifies the array every second, and we expect the directive output to mirror this change.

Here's the Javascript to wire this up:

```javascript
angular.module("myApp", [])
	.controller("details", function($interval){

		var ctrl = this;
		var count = 1;

		//expose our name and numbers:
		ctrl.name = "James";
		ctrl.numbers = [];

		//periodically change numbers array:
		var i = $interval(function(){
			ctrl.numbers.push(count++);
			if(count > 10) $interval.cancel(i);
		}, 1000);

	})
	.directive("inner", function(){
		return {
			//declaring scope like this isolates the directive
			//from the outside world and from information in
			//parent scopes (prototypically at least)
			scope: {
				//"watch" the name property on both sides:
				name: "=",
				//add * to shallow watch an object rather than by ref:
				inputNumbers: "=*numbers"
			},
			controller: function($scope){

				$scope.$watchCollection("inputNumbers", function(n){

					if(!angular.isArray(n)) return;

					//add half increments too so numbers arr is 1,1.5,2,2.5,3,3.5..
					if(n.length && n[n.length-1] == Math.floor(n[n.length-1])){
						n.push(n[n.length-1]+0.5);
					}

					$scope.numbers = n.join(", ");

				});

			},
			//this is what we render inside the div with "inner" on:
			template: "<div>{{name}}: {{numbers.length? numbers : 'No Numbers'}}</div>"
		};
	});
```

For someone that's been doing Angular for a while, this looks pretty straight forward, but to the newcomer there are a number of new concepts right off the bat. First, dependency injection; the fact that the argument order on the controller functions and directive function is irrelevant; arguments are asked for by name (see my post [here][angular-di] for details on this). Next, the Angular digest cycle, which is why we need to `watch` things in order that our application can react to things changing properly.

> The digest cycle is a bit of a brick wall for beginners, and it hits you hard as soon as you want to create custom elements. Fundamentally, Angular must store a list of all values that it needs to _watch_ for changes. Each time a digest is triggered (normally by some directive), Angular checks all values it is watching and performs the necessary action attached to each watch (for example updating HTML, or in the above case noticing that `numbers` changed and updating our string representation of it.)

The set of services and directives that Angular includes handle triggering digests for you when you'd commonly want Angular to react to changes, such as on user input, or when promises resolve. If you want to handle some custom behaviour that Angular does not provide a directive for, or react to changes outside of Angular, you'll need to trigger one yourself to tell Angular to make sure everything is up to date. The same is true for watches; Angular directives handle watching things in many cases, but there are times when you'll want to tell Angular to watch something explicitly.

Time to see how we'd do the same thing in React.

### React

Unlike Angular, in which you declare your layout in HTML (on the whole), React mangles the HTML and logic together in Javascript land. In HTML, we just need an element to mount our React component into, for example:

```
<div id="react-talk-twoway"></div>

```

The meat of our application is then in Javascript:

```tsx
var Outer = React.createClass({

	getInitialState: function(){
		return {
			name: "James",
			count: 1,
			numbers: []
		};
	},

	componentDidMount: function(){
		var self = this;
		self.__interval = setInterval(function(){

			var n = self.state.numbers,
			    c = self.state.count;

			self.setState({
				numbers: n.concat([c]),
				count: c+1
			});

			if(c+1 > 10) clearInterval(self.__interval);

		},1000);
	},

	updateNumbers: function(newNumbers){
		this.setState({
			numbers: newNumbers
		});
	},

	componentWillUnmount: function(){
		clearInterval(this.__interval);
	},

	render: function(){
		var s = this.state;
		return (
			<div>
				{JSON.stringify(this.state.numbers)}
				<Inner numbers={s.numbers} name={s.name} onUpdate={this.updateNumbers} />
			</div>
		);
	}

});

var Inner = React.createClass({

	propTypes: {
		numbers: React.PropTypes.array,
		name: React.PropTypes.string,
		onUpdate: React.PropTypes.func
	},

	getDefaultProps: function(){
		return {
			numbers: [],
			name: "No Name",
			onUpdate: function(){}
		};
	},

	getInitialState: function(){
		return {};
	},

	componentWillReceiveProps: function(nextProps){

		var n = nextProps.numbers;

		//add half increments too so it's like 1,1.5,2,2.5,3,3.5..
		if(n.length && n[n.length-1] == Math.floor(n[n.length-1])){
			n = n.concat([ n[n.length-1]+0.5 ]);
			this.props.onUpdate(n);
		}

		this.setState({
			numberString: n.join(", ")
		});

	},

	render: function(){
		return (
			<div>{this.props.name}: {this.state.numberString || 'No Numbers'}</div>
		);
	}

});

React.render(
	<Outer />,
	document.getElementById("react-talk-twoway")
);
```

React has fewer concepts to learn than Angular. Instead of controllers, directives, services and so on, we just have components. Here, we have two React components named `Inner` and `Outer`.

We make use of Reacts JSX transformer to write XML style syntax to represent the output HTML. It is easy to add the transformer to compile this into regular function calls into your build process, and I find JSX much easier to work with (but using it is optional if you'd prefer regular function calls).

> React classes consist of a mandatory `render` function, which - you guessed it - renders some HTML. Other methods are optional and have pretty clear names; `componentWillReceiveProps` is called whenever the the values being passed to the component are about to change, `getInitialState` gets the initial component state, and `propTypes` is a nice optional extra that throws warnings into your console if values passed into your component don't match what's expected by it (and which is disabled in production mode for performance).

In our example, Our `Outer` component contains an instance of our `Inner one`, and passes it the name and numbers array, along with an `onUpdate` function.

Angular bindings are by default two way. In the angular example, changes inside the `inner` directive to the numbers array or name string are propagated to the outside world (by a series of watches angular creates behind the scenes). This Allows a form of child to parent communication not present in React, but also one I tend to avoid in Angular as it makes the flow of data a little less clear.

In React, the common alternative to two way binding is to pass a callback function to the child element that the child can then fire on some event to notify the parent of it. We do this with our `onUpdate` function.

While more verbose, I think it is a lot easier for a beginner to grasp the React example; it has a simplicity to it that I can't help but respect.

## Communication Between Components: Horizontal

Fairly often you'll want some form of horizontal communication or coordination between your different components. For example, if something changes on the backend an event might fire to the front end which your components will want to react to. Alternately you might just have a menu bar on one side of your application and various things that want to react or otherwise communicate with it on the other (say when things are dragged onto it perhaps).

Here's one way to do that in Angular that is quite widely applicable to a range of problems. First, the HTML:

```html
<div ng-app="myApp">
	<div ng-controller="countDisplay as ctrl">
		Count: {{ctrl.count}}
	</div>
	<div ng-controller="countControls as ctrl">
		<button ng-click="ctrl.increment()">Increment</button>
		<button ng-click="ctrl.decrement()">Decrement</button>
	</div>
</div>
```

Here we have two controllers, each responsible for separate parts of the DOM. One controller is responsible for showing us a count, and another is responsible for modifying that count. Here's the Javascript to wire this up:

```javascript
angular.module("myApp", [])
	.factory("counter", function($rootScope){

		var counter = 0;
		return {
			getCount: function(){
				return counter;
			},
			setCount: function(value){
				counter = value;
				$rootScope.$emit("counterChanged")
			},
			onChange: function($scope, fn){
				var off = $rootScope.$on("counterChanged", fn);
				$scope.$on("$destroy", off);
				return off;
			}
		};

	})
	.controller("countDisplay", function($scope, counter){

		var ctrl = this;
		ctrl.count = counter.getCount();
		counter.onChange($scope, function(){
			ctrl.count = counter.getCount();
		});

	})
	.controller("countControls", function($scope, counter){

		this.increment = function(){
			counter.setCount( counter.getCount()+1 );
		}
		this.decrement = function(){
			counter.setCount( counter.getCount()-1 );
		}

	});
```

First we create a service using the `factory` function. This service exposes functions to alter and retrieve the count, and a function which fires a provided callback each time it is changed (using the built in `$rootScope` event emitting functionality to make quick work of this). Using a change event rather than watchers is more performant and scales well to working with large amounts of data or asynchronous data for instance.

Thanks to the dependency injection magic, our new service can now be asked for by name by each of the two controllers, so we make short work of using the simple interface it provides to expose functions to increment and decrement in one controller, and keep it up to date in the other.

As an aside, the `ng-app` directive which links your angular app of the same name to that element, can only be used once per page, but luckily the alternative approach in Javascript is very easy and would look something like:

```javascript
angular.bootstrap(
	document.getElementById("counter"),
	['angularCounter']
);
```

Which is somewhat similar to the React `render` function which achieves the same goal. Use this if you want to create widgets rather than full page apps to avoid conflict. Your widgets will be entirely isolated from each-other in Angular land and thus cannot share services and the like.

### React

How would we do the same in React? First, there is nothing built in we can use as a channel to send data, so I'll whip one together quickly:

```javascript
var MixinChannel = function(){
	var subscribed = [];
	return {
		createMixin: function(){
			var offs = [];
			return {
				onChange: function(fn){
					subscribed.push(fn);
					var off = function(){
						var idx = subscribed.indexOf(cb);
						if(idx !== -1) subscribed.splice(idx,1);
					}
					offs.push(off);
					return off;
				},
				componentWillUnmount: function(){
					for(var i = 0; i < offs.length; i++){
						offs[i]();
					}
				}
			};
		},
		fire: function(){
			for(var i = 0; i < subscribed.length; i++){
				subscribed[i].apply(null,arguments);
			}
		}
	}
}
```

I called this a `MixinChannel` because instances of it expose a `createMixin` function which can be used to provide a _mixin_ to React components. Mixins are a nice way to compose general purpose functionality into React components, extending the properties of the React component and intelligently merging duplicate uses of lifecycle methods. With this created, we can build the core of our app.

First, the obligatory HTML to mount our components into. We'll mount two separate components to artificially make our job of communicating more difficult, so we'll have two elements:

```html
<div id="react-counter-display"></div>
<div id="react-counter-controls"></div>
```

Finally, the Javascript to make it tick.

```tsx
var Counter = function(){
	var count = 0;
	var chan = MixinChannel();
	var mixin = chan.createMixin("onCounterChanged");
	return {
		getCount: function(){
			return count;
		},
		setCount: function(value){
			count = value;
			chan.fire();
		},
		onCounterChanged: mixin.onChange,
		componentWillUnmount: mixin.componentWillUnmount
	};
}

var myCounter = Counter();

var CounterControls = React.createClass({
	mixins: [myCounter],
	incrementCount: function(){
		this.setCount(this.getCount()+1);
	},
	decrementCount: function(){
		this.setCount(this.getCount()-1);
	},
	render: function(){
		return (
			<div>
				<button onClick={this.incrementCount}>Increment</button>
				<button onClick={this.decrementCount}>Decrement</button>
			</div>
		);
	}
});

var CounterDisplay = React.createClass({
	mixins: [myCounter],
	getInitialState: function(){
		return {count: this.getCount()};
	},
	componentDidMount: function(){
		var self = this;
		self.onCounterChanged(function(){
			self.setState({count: self.getCount()});
		});
	},
	render: function(){
		return (
			<div>Count: {this.state.count}</div>
		);
	}
});

React.render(
	<CounterDisplay/>,
	document.getElementById("react-counter-display")
);
React.render(
	<CounterControls/>,
	document.getElementById("react-counter-controls")
);
```

First, we create a `Counter` factory. From that we make one counter instance, which plays the role of our counter service in Angular, and provide it as a mixin to our `CounterControls` and `CounterDisplay` components. This endows them with the methods exposed in the counter instance. Our mixin automatically removes subscribed events if the component is unmounted, preventing leaks.

Once again, the React version is more verbose, especially as we had to write some code in React that Angular provides for us. This is the sort of thing that leads to React being referred to as "just the directive part of Angular". While there is truth to this, I do believe that much of what Angular provides is to help manage its digest and watch cycle without exposing it to the user too much, something React does not require. Other parts can be build relatively easily or pulled in from third party sources.

## More Advanced Components: A Tree

Let's try something more advanced.

Something I have encountered a need for a number of times is the need to render data that takes the form of a tree-like structure in my HTML. Such data might look like this:

```javascript
[
	{
		title: "Folder 1",
		children: [
			{
				title: "Sub 10"
			},
			{
				title: "Sub 11"
			},
			{
				title: "Sub 12",
				children: [
					{
						title: "Sub 100"
					},
					{
						title: "Sub 200"
					}
				]
			}
		]
	},
	{
		title: "Folder 2",
		children: [
			{
				title: "Sub 20"
			}
		]
	}

]
```

I have kept it to a bare minimum, expecting titles to be unique (rather than a separate id property as would be more common) and children to be optional for any given item. The aim is render a tree with items that can be individually expanded and contracted.

The recursive nature of such a structure can easily trip you up in Angular initially. Let's start with a very simple approach for one-off rendering of some tree structure in Angular. Here's the HTML:

```html
<div ng-app="myApp">
	<script type="text/ng-template" id="tree-template">
		<div ng-repeat='item in items track by item.title'>
			{{item.title}}
			<button ng-show="item.children" ng-click="showChildren = !showChildren">
				{{ (showChildren? 'Hide' : 'Show')+' Children' }}
			</button>
			<tree ng-if='item.children && showChildren' items='item.children'></tree>
		</div>
	</script>
	<div ng-controller="treeCtrl as ctrl">
		<tree items="ctrl.tree"></tree>
	</div>
</div>
```

Here, we create an angular template called `tree-template`. We also make use of a `tree` directive we have created to help with the recursive aspect of this. The Javascript then looks like this:

```javascript
angular.module("myApp", [])
	.directive("tree", function($compile,$templateCache){
		return {
			//this directive uses a new scope, one which inherits
			//properties from parent scopes rather than is isolated:
			scope: true,
			link: function(scope,elem,attrs){
				scope.items = scope.$eval(attrs.items);
				if(!scope.items.length) return;
				var html = $compile($templateCache.get("tree-template"))(scope);
				elem.append(html);
			}
		};
	})
	.controller("treeCtrl", function(){
		this.tree = [/* tree structure goes here */];
	});
```

While concise, we make use of some new concepts in Angular.

First, we need to `$eval` the "item.children" attribute string provided to the directive, assigning the result to `scope.children`. This makes use of the fact that the directive scope prototypically inherits from its parent.

The `$templateCache` allows access to any templates Angular has picked up (as a result of their _text/ng-template_ type) and stored away (in string form), so we use that to get at our `tree-template` HTML.

The `$compile` service is what Angular uses to turn an almost normal looking string of HTML with Angular directives and curly braces sprinkled about, into a ready-to-view HTML fragment that will react to changes and whatnot.

We give `$compile` our HTML string, and then link it to our tree scope, making anything on that scope usable inside the HTML fragment. We only perform the compilation if the list has children, otherwise we get stuck in an infinite loop whereby every attempt to compile the template leads to another attempt to compile the tree directive, and so on.

This is very basic but renders the tree structure complete with buttons that allow it to be expanded and contracted, as well as allowing functions declared on scopes above where the tree directive is used to be used inside it.

Many things however are less than ideal. Changes aren't properly watched for one thing. Fortunately, I wanted to make use of a directive like this in work, so I turned it into something more production ready. Here's what a fully capable `tree` directive might look like in Angular:

```javascript
App.directive(NAME, function(){
	return {
		restrict: "AE",
		scope: true,
		transclude: true,
		controller: ["$scope","$element","$attrs","$transclude",
			function( $scope,  $element,  $attrs,  $transclude){

			if("children" in $attrs) $scope.__treeOpts = {
				transcludeFn: $transclude,
				noWatch: "noWatch" in $attrs? true : false,
				class: $scope.$parent.$eval($attrs.itemClass) || "",
				key: $attrs.key || false,
				ref: $attrs.ref || "item"
			};

			if(!$scope.__treeOpts) {
				throw Error(NAME+":: tree declared with 'children' attr but not a child tree.");
			}

			var noWatch = $scope.__treeOpts.noWatch;
			var turnOffWatch = $scope.$parent.$watchCollection($attrs.items, function(n){
				var bIsArray = n instanceof Array;
				$scope.items = bIsArray? n : [];
				if(bIsArray && noWatch) turnOffWatch();
			});

		}],
		template: "<div ng-repeat='item in items track by (__treeOpts.key? item[__treeOpts.key] : $index)' ng-class='__treeOpts.class' jw-tree-item='item'></div>"
	};
});

App.directive("jwTreeItem", function(){
	return {
		restrict: "A",
		scope: true,
		controller: ["$scope","$element","$attrs", function($scope,$element,$attrs){

			if(!$scope.__treeOpts) throw Error(NAME+":: TreeItem not beneath a Tree!");

			var noWatch = $scope.__treeOpts.noWatch;
			var turnOffWatch = $scope.$parent.$watch($attrs.jwTreeItem, function(n){
				var bIsUndefined = typeof n == "undefined";
				$scope[$scope.__treeOpts.ref] = n;
				if(bIsUndefined && noWatch) turnOffWatch();
			});

			$scope.__treeOpts.transcludeFn($scope, function(elem){
				$element.append(elem);
			});
		}],
	};
});
```

And here's how it might be used to render the same tree structure as above without any extra Javascript:

```html
<div ng-controller="treeCtrl as ctrl">
	<tree ref="item" key="title" items="ctrl.tree">
		{{item.title}}
		<button ng-show="item.children" ng-click="showChildren = !showChildren">
			{{ (showChildren? 'Hide' : 'Show')+' Children' }}
		</button>
		<tree children items='item.children' ng-if='item.children && showChildren'>
		</tree>
	</tree>
</div>
```

This version will react to any changes as you'd expect. You pass to it the HTML you'd like an item to be rendered with, which can itself include a reference to the tree directive to point out where the children should live and what property to find the children in.

This highlights one of the best/worst points in Angular I think. You can create components with beautiful interfaces that are as general purpose as you could want, but in order to do so you have to understand a number of concepts quite well. Things like scope inheritance for example can easily lead to hard to find bugs.

### React

Let's look at a similar implementation in React. We'll omit the obligatory HTML element and jump to the Javascript:

```tsx
//our 'tree' component, which is really just repeating over
//a list of elements and rendering the provided component
//for each:
var Tree = React.createClass({
	propTypes: {
		items: React.PropTypes.array.isRequired,
		template: React.PropTypes.func.isRequired,
		keyProp: React.PropTypes.string.isRequired
	},
	render: function(){
		var keyProp = this.props.keyProp;
		var items = this.props.items;
		var Child = this.props.template;
		return (
			<div>
			{
				items.map(function(item){
					return <Child key={item[keyProp]} item={item} />
				})
			}
			</div>
		);
	}
});

//the equivalent of our custom HTML to use for items in
//the tree, which calls back to the tree itself for the
//recursive rendering:
var TreeItem = React.createClass({
	propTypes: {
		item: React.PropTypes.shape({
			title: React.PropTypes.string.isRequired,
			children: React.PropTypes.array
		})
	},
	getInitialState: function(){
		return {childrenVisible: false};
	},
	toggleChildren: function(){
		this.setState({childrenVisible: !this.state.childrenVisible});
	},
	render: function(){
		var item = this.props.item;
		var visible = this.state.childrenVisible;
		return (
			<div>
				{item.title}
				{item.children?
					<button onClick={this.toggleChildren}>
						{(visible? 'Hide' : 'Show')+' Children'}
					</button>
				:false}
				{visible?
					<Tree items={item.children || []} keyProp="title" template={TreeItem} />
				:false}
			</div>
		);
	}

});

React.render(
	<Tree items={window.tree} keyProp="title" template={TreeItem}/>,
	document.getElementById("someElement")
);
```

The React version uses the same basic React concepts we've made use of in prior examples. It's hardly even a tree component, being more general than that, but the end result is the same. In our more advanced Angular version, we pass a bunch of properties "under the hood" to each child tree, so we don't need to redeclare them. This is done by making use of scope inheritance. React prefers to be explicit, and so we have to pass properties explicitly down through child components.

On the one hand, the React example is much clearer and understandable. On the other, Angular has enabled me to create a much more general, powerful and reusable component. That is not to say that you could not create an equally powerful component in React, more that there is not an immediately obvious _Reacty_ approach to doing so.

## Multiple Transclusion

Transclusion is a fancy term for essentially ripping out child nodes of a component so that we can reuse them elsewhere. We make use of it in our angular tree example above to rip out the HTML we intend to use to render each tree item from inside the outermost tree directive so that we can make use of it instead in place of each child item in the new HTML structure that the tree directive creates.

A very simple example in Angular might take some HTML such as:

```html
<fancybox>
	<span>Hello</span>
	<img src="someimage.jpg"/>
</fancybox>
```

And, given a simple implementation of fancybox that looks something like:

```javascript
angular.directive("fancybox", function(){
	return {
		transclude: true,
		template: '<div class="fancybox"><div class="fancybox-inner" ng-transclude></div></div>'
	};
});
```

Render something like the resulting HTML:

```html
<fancybox>
	<div class="fancybox">
		<div class="fancybox-inner">
			<span>Hello</span>
			<img src="someimage.jpg"/>
		</div>
	</div>
</fancybox>
```

We have ripped out the child HTML of `fancybox` and reinserted it into a new place as determined by our use of `ngTransclude` above. This sort of thing is really useful for creating general purpose components of all types, from ones that automatically centre content for you to reusable popups, modals and so on. Basic transclusion is therefore very easy to do in both React and Angular.

The next step from this is multiple transclusion, where you'd like to provide a directive several HTML fragments, each to be ripped out and replaced at different locations in the new template. Let's first look at how we'd do this in Angular.

We'll start with this HTML:

```html
<div ng-controller="fancyWidgetCtrl as ctrl">
	<fancy-widget>
		<span part="title-part">
			Hello There <em>{{ctrl.name}}</em>
		</span>
		<span part="content-part">
			This is a {{ctrl.verb}} widget!
		</span>
	</fancy-widget>
</div>
```

This has two parts, "title-part" and "content-part", that we want to rip out and replace in some new locations in our final HTML. Each part also makes use of some angular variables available from our controller aliased as `ctrl`. Here's the Javascript:

```javascript
angular.module("fancyWidget", [])
	.directive("fancyWidget", function(){
		return {
			scope: {},
			transclude: true,
			controller: function($scope,$element,$transclude){

				//just use the scope the content was pulled from.
				$transclude(function(el){
					$element.find("title-part").replaceWith(el.filter("[part=title-part]"));
					$element.find("content-part").replaceWith(el.filter("[part=content-part]"));
				});

			},
			template: "<h3>Title: <title-part/></h3><div>Content: <content-part/></div>"
		};
	})
	.controller("fancyWidgetCtrl", function(){
		this.name = "James";
		this.verb = "fancy";
	});
```

We define a very small template inline, though we'd provide a `templateUrl` for anything significant and keep the template in a separate file. In the template, we replace our `title-part` and `content-part` elements with the parts of corresponding names from the original HTML. For this, we make use of a little jQuery to pluck the relevant parts out of the transcluded result and place them as desired.

One of the nice things about Angular is that once you define directives like this, or the tree directive above, you can get back to working in just the HTML, focusing on what you want and having the directives do the leg work for you behind the scenes.

Multiple directives can be included on the same element to confer different properties. Often one might combine directives which handle events like drag and drop with those that perform some HTML transformation like the above. This style of composition is not available to us in React.

### React

How does the React version compare? Here it is:

```tsx
var FancyWidget = React.createClass({
	render: function(){

		//find the chldren we want to use:
		var title = false, content = false;
		React.Children.forEach(this.props.children, function(child){
			if(child.props.part == "title") title = child;
			else if(child.props.part == "content") content = child;
		});

		return (
			<div>
				<h3>Title: {title}</h3>
				<div>Content: {content}</div>
			</div>
		);

	}
});

var App = React.createClass({
	getDefaultProps: function(){
		return {
			name: "James",
			verb: "fancy"
		}
	},
	render: function(){
		return (
			<FancyWidget>
				<span part="title">Hello there <em>{this.props.name}</em></span>
				<span part="content">This is a {this.props.verb} widget!</span>
			</FancyWidget>
		);
	}
})

React.render(
	<App />,
	document.getElementById("react-fancy-widget")
);
```

The React version makes use of a helper provided to loop over child elements, conveniently provided as a `children` prop in the same way that other attributes are, and pluck out the relevant elements based on some attribute.

We also create an `App` component to take the place of the outermost controller in the Angular version, and show that we can work with dynamic content just as easily.

Both versions are actually quite simple, although the React version pips it for clarity once again I think (though it would be a close call in this case). Both versions are a similar length this time round.

The Angular version is more clunky if jQuery is not made use of. This is one of the minor gripes I have with Angular; it comes with a trimmed down version of jQuery called jqLite, and thus wraps all elements it exposes to you in this class, but jqLite lacks many very useful features of jQuery and so I tend to quickly feel obliged to include the full version to avoid jumping through hoops. In React I have avoided any need for either, and what DOM manipulation I have had to do in other projects I have always been happy enough with native browser methods.

## Encapsulation and Namespacing

One of the significant differences that might have become apparent through some of the examples is that of namespacing. In Angular (within a single app), every directive you create or pull in from elsewhere must be given a unique name so that they can be used separately in the HTML. The standard solution to this is prefixing directives in a particular package. This is one of the tradeoffs for getting to write "normal" HTML; you can only have one element of a given name.

Because React does not use HTML, it does not have this issue. React components are just functions, and so their name is irrelevant. You can easily hide away components so that they can't be used elsewhere in your application, for example:

```tsx
var HelloWorld = (function(){

	var HiddenComponent = React.createClass({
		render: function(){
			return <div>Hello</div>
		}
	});

	return React.createClass({
		render: function(){
			return (
				<div><HiddenComponent/> World</div>
			);
		}
	});

}());
```

We use a self executing function scope to hide our `HiddenComponent` from the outside world, exposing only those components we want. Using something like Browserify we'd just place the components we want to make public on `module.exports` and anything else can use only what it needs without any fear of conflict.

Looking at the component definitions themselves, React forces you to pass props explicitly to child components if you want them to be available there, whereas in angular the default behaviour is to inherit properties from parent scopes. This scope inheritance is often a stumbling block, for instance, what does the following do when you click the "Show Hello" button?

```html
<div ng-init="hidden = false; showButton = true">
	<div ng-if="showButton">
		<button ng-click="hidden = !hidden">Show Hello</button>
	</div>
	<div id="hello" ng-if="!hidden">Hello!</div>
</div>
```

The answer is, nothing. The `ng-if` directive on the element above the button actually creates a new scope, and then clicking the button sets the hidden property on that scope, rather than its parent, which is what the `#hello` div is looking at to determine whether to be hidden or not. A tiny change from `ng-if` to `ng-show` fixes the problem, as `ng-show` does not create a new scope:

```html
<div ng-init="hidden = false; showButton = true">
	<div ng-show="showButton">
		<button ng-click="hidden = !hidden">Show Hello</button>
	</div>
	<div id="hello" ng-if="!hidden">Hello!</div>
</div>
```


Even now I am occasionally tripped up with scope inheritance niggles like this. This sort of thing make the initial learning curve of Angular much steeper. Angular does provide things like the "controller as" syntax to help avoid the issue by forcing you specify on which scope you want values from explicitly.

When you understand scope inheritance, you can use it to make more beautiful directives by passing data behind the scenes rather than explicitly as attributes on each child as you would do in React. It can however be a source of hard to spot bugs, as in the HTML it can be almost impossible to know exactly where new scopes are created without a good understanding of every directive in use.


# Conclusions

Having looked at several examples of real use cases and how one might achieve them in each framework, I hope I have given you some insight into the pros and cons of each.

In general, each example seems to echo the statement that Angular can produce nicer interfaces in fewer lines of code than React, but at the cost of simplicity. Creating directives in Angular can be confusing at first not just because of the sudden need to understand various concepts that were previously tucked out of sight, but because of the multitude of ways that you can go about it. It is easy to get stuck wondering when is best to use the `link` method over the `controller` method for instance, since both provide access to the same properties and often either will be fine.

React does not give you so much out of the box, but also does not need so much. The React lifecycle is simple and explicit, and so we have no use for custom `$timeout`s, `$interval`s, custom event handlers like `ngClick` and the like, or a custom promise implementation; we just use native methods. Instead of `ngRepeat` we just use an `array.map(function(){..})` to repeat some HTML for a list. Instead of `$q` we use the native `Promise` interface (with a shim for older browser support if necessary). Instead of jQuery we use native selectors where we still have need for them.

The separation of HTML and Javascript provided by Angular is still a point of debate for me. Working with others, I love that I can create a directive to make something tick, while someone else can work on the HTML to make the thing look nice (so long as we agree on an interface). While logic and HTML are inseparable in both, Angular allows you to abstract a lot of that logic away into directives so that it is just possible to focus on the presentation layer. In React, you always have to consider the your layout and logic, but then the way in which you define components in React also means that the two are very much more tightly coupled. That said, it is worth checking out something like [React Templates][react-templates], which seems to take steps toward offering the same level of separation in React.

I will be favouring React for my personal projects, but then that is partly just because I have used Angular so much already and want to play with something new. Partly however, I like that while Angular took a fair bit of time to really get to grips with, one read over the React tutorials gave me everything I need straight off the bat. I appreciate elegance in code, and while React is more verbose, I think it is also more the more elegant of the two.

# The Future

One thing worth mentioning with regard to either framework is where they are heading. Both frameworks are still on a path of rapid evolution. React 0.13 brings a new way of defining components using ES6 classes rather than the `createClass` method used above, though it currently lacks support for mixins and does not have a way to define static properties of classes yet. When it is time to make the change, it will be an easy transition to make as the interface is relatively unaffected.

Angular version 2, which is still under development, is also moving toward using an approach based on ES6 classes, however it is also a complete framework rewrite, which caused a bit of an uproar in the community. The upgrade path from Angular 1.x to 2.x is currently unclear and at the very least it looks like a fairly significant amount of work to bring your code forward. There are at least hints of a migration path whereby Angular 2.0 code will be able to be used alongside 1.x code in apps, allowing a more gradual shift in your codebase from one platform to the other.

In this sense, React is the safer choice if you are starting out with a new framework, although I expect that many of the concepts learned in Angular 1.x will give you a head start with [Angular 2][angular2] if it looks like your kind of thing.

[plunkr]: http://plunkr.co/edit
[example-code]: plunkr.html
[angular-di]: ./posts/angular-services-and-di-demystified/index.md
[react-templates]: http://wix.github.io/react-templates/
[angular2]: http://angular.io














