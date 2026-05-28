/*
Copyright (c) 2013, James Wilson (http://unbui.lt)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

function State(ref)
	{
	if(!(this instanceof State)) return new State(ref);

	var funcs = {};
	var old = copy(ref);
	
	//get instance of thing for comparison:
	var type = (function()
		{
		var toString = Object.prototype.toString;
	
		function checkType(thing)
			{
			return toString.call(thing);
			}
		
		checkType.object = checkType({});
		checkType.function = checkType(function(){});
		checkType.null = checkType(null);
		checkType.array = checkType([]);
		checkType.boolean = checkType(false);
		checkType.undefined = checkType(undefined);
	
		return checkType;
		})();
	
	//standardise path:
	function resolvePath(path)
		{
		return path[0] != "."? "."+path : path;
		}
	
	//copies anything passed to it:
	function copy(o)
		{
		if(typeof o != "object" || o === null) return o;
		var r = (o instanceof Array)? [] : {};
		for(var i in o)
			{
			r[i] = copy(o[i]);
			}
		return r;
		}
	
	//boolean comparison between two things:
	function booleanCompare(one, two)
		{
		if(type(one) != type(two)) return false;
		
		if(type(one) == type.function)
			{
			return one.toString() == two.toString();
			}
		else if(typeof one != "object") //less strict comparison
			{
			return one === two;
			}
		else //must be something we can iterate over then:
			{
			for(var i in one) if(booleanCompare(one[i], two[i]) == false) return false;
			for(var i in two) if(typeof one[i] == "undefined") return false; 
			return true;
			}
		}
	
	//check for changes between two objects:
	function compare(one, two, func, path)
		{
		path = path || "";

		var one_is_obj = type(one) == type.object;
		var two_is_obj = type(two) == type.object;
		
		//if neither are objects, normal comparison:
		if(!one_is_obj && !two_is_obj) 
			{
			if(!booleanCompare(one,two))
				{
				return true;
				}
			else return false;
			}
		
		var temp, base_difference = false, inner_difference = false, inner_path;
		
		//if first is object, look for internal differences.
		if(one_is_obj) for(var i in one)
			{
			inner_path = path+"."+i;
			temp = two_is_obj? two[i] : undefined;

			inner_difference = compare(one[i], temp, func, inner_path);
			if(inner_difference) 
				{
				base_difference = true;
				func(inner_path, one[i], temp);
				}
			}
			
		//look for any remaining differences not spooted from first obj:
		if(two_is_obj) for(var i in two)
			{
			inner_path = path+"."+i;
			temp = one_is_obj? one[i] : undefined;
			if(one_is_obj && typeof one[i] != "undefined") continue;

			inner_difference = compare(temp, two[i], func, inner_path);
			if(inner_difference) 
				{
				base_difference = true;
				func(inner_path, temp, two[i]);
				}
			}
			
		return base_difference;
		}
		
	//from an object, fire callback with every path and associated value:
	function forEveryPath(obj, func, path)
		{
		if(type(obj) != type.object) return;
		
		for(var i in obj)
			{
			var val = obj[i], new_path = path? path+"."+i : i;
			if(type(val) == type.object) forEveryPath(val, func, new_path);
			else func(new_path, val);
			}
		}
		
	//set a function to be executed for a given path, or a group of funcs if path is object:
	function setFunc(path, func)
		{
		if(type(path) == type.object)
			{
			//if path is object of funcs, recurse over, setting all:
			forEveryPath(path, setFunc);
			}
		else if(typeof path == "function")
			{
			//if path is function, set global func to it:
			func = path;
			path = ".";
			}
		
		//if params are both correct, set the func:
		if(typeof func == "function" && typeof path == "string")
			{
			funcs[resolvePath(path)] = func;
			}
		}
		
	//remove function from path (or functions if object is provided)
	function removeFunc(path)
		{
		if(typeof path == "object")
			{
			forEveryPath(path, removeFunc);
			}
		else if(typeof path == "string")
			{
			delete funcs[resolvePath(path)];
			}
		}
	
	
	//sets path to value in a copy of the original object, then compares copy to original
	function setValue(path, value)
		{
		//if path is an object, assume we want to change everything:
		if(typeof path != "string")
			{
			value = path;
			path = "";
			}
		
		var temp = old; //temp is now the old state
		var modified; //modified becomes the new state

		//set the value, if path is a valid string:
		if((path = path.replace(/^\./, "")).length)
			{
			modified = type(old) == type.object? copy(old) : {}; 
			var path_arr = path.split("."), i, current_path, current_ref = modified;

			for(i = 0; i < path_arr.length-1; ++i)
				{
				current_path = path_arr[i];
				current_ref = type(current_ref[current_path]) == type.object?
					current_ref[current_path] : (current_ref[current_path] = {});
				}
				
			var value_name = path_arr[path_arr.length-1];
			current_ref[value_name] = copy(value);
			}
		//if path is a "." or empty, set the whole thing
		else modified = copy(value);
		
		//modified becomes the "old" value now.
		old = modified;
		
		//make a copy of modified to pass out (so people cant screw up internals):
		var copy_of_modified = copy(modified);
		
		//run the check:
		var difference = compare(temp, copy_of_modified, function(str, old_val, new_val)
			{
			var f = funcs[str];
			if(typeof f == "function") f(old_val, new_val, str);
			});
		
		//if there's a difference, fire the global function too:
		if(difference)
			{
			var f = funcs["."] || funcs[""];
			if(typeof f == "function") f(temp, copy_of_modified, "."); 
			}	
		}
		
	//gets value given path string:
	function getValue(path)
		{
		if(typeof path != "string") path = "";
		
		path = path.replace(/^\./, "");
		if(!path.length) return copy(old);

		var lookups = path.split("."), l = lookups.length, val = old;
		for(var i = 0; i < l; i++)
			{
			if(type(val) != type.object) return undefined;
			val = val[lookups[i]];
			}
		return copy(val);
		}
		
	//trigger function assigned to state, or all:
	function trigger(path)
		{
		if(typeof path != "string")
			{
			var val;
			for(var i in funcs)
				{
				val = getValue(i);
				funcs[i](val, val);
				}
			return;
			}
		if(path[0] != ".") path = "."+path;
		var f = funcs[path], val = getValue(path);
		if(typeof f == "function") f(val, val);
		}
		
	//public interface:
	this.setFunction = setFunc;
	this.removeFunction = removeFunc;
	this.getValue = getValue;
	this.setValue = setValue;
	this.triggerFunction = trigger;
	}



//monitors json cookies created by Express.js for changes:
function ExpressCookieState()
	{
	if(!(this instanceof ExpressCookieState)) return new ExpressCookieState();

	//specific to node.js cookies (possibly connect middleware too):
	function parseCookie()
		{
		var cookie_bits = decodeURIComponent(document.cookie).split(";");
		var output = {}, value, pieces;
		for(var i = 0; i < cookie_bits.length; i++)
			{
			pieces = cookie_bits[i].trim().split("=");
			if(pieces.length < 2) continue;
			value = pieces[1];
			//remove secure signing:
			value = value.indexOf("s:") == 0?
				value.slice(2).replace(/\.[^.]*$/,"") :
				value;
			//parse explicit json:
			value = value.indexOf("j:") == 0?
				JSON.parse(value.slice(2)) :
				value;
			output[pieces[0]] = value;
			}
		return output;
		}
	
	var cookie_str = document.cookie;
	var state = parseCookie();
	var m = new State(state);
	
	this.check = function()
		{
		if(cookie_str == document.cookie) return this;
		cookie_str = document.cookie;
		state = parseCookie();
		m.setValue(state);
		return this;
		}
	this.setFunction = function(path, func)
		{
		m.setFunction(path, func);
		return this;
		}
	this.removeFunction = function(path)
		{
		m.removeFunction(path);
		return this;
		}
	this.getValue = function(str)
		{
		return m.getValue(str);
		}
	this.triggerFunction = function(str)
		{
		m.triggerFunction(str);
		return this;
		}
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
