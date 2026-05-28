/*
Redirection engine. Inspired by Express.js.

Copyright (c) 2013, James Wilson (http://unbui.lt)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

function Engine()
	{
	function parseQuery(str)
		{
		if(typeof str != "string" || str.length == 0) return {};
		var s = decodeURIComponent(str).split("&");
		var s_length = s.length;
		var bit, query = {}, first, second;
		for(var i = 0; i < s_length; i++)
			{
			bit = s[i].split("=");
			first = bit[0];
			if(first.length == 0) continue;
			second = bit[1];
			if(typeof query[first] == "undefined") query[first] = second;
			else if(query[first] instanceof Array) query[first].push(second);
			else query[first] = [query[first], second]; 
			}
		return query;
		}
	
	return {
		routes: [],
		add: function(route)
			{
			var vars = [], no_route = false;
			//if string, convert to regexp source:
			if(typeof route == "string")
				{
				//remove first "/" if present (it is optional):
				if(route.length && route[0] == "/") route = route.slice(1);
			
				//find :words in string:
				var r = /\{([^}]+)\}/g, arr
				while((arr = r.exec(route)) !== null)
					{
					vars.push(arr[1]);
					}

				//convert to regexp source:
				route = "^\\/?" + route.replace(/\//g, "\\/").replace(/\{[^}]+\}/g, "([^/]+)") + "\\/?$";
				}
			//if regexp, convert to regexp source:
			else if(route instanceof RegExp)
				{
				route = route.source;
				}
			//if route is anything else, ignore it:
			else
				{
				no_route = true;
				}
		
			//add functions passed in wih route to an array
			var funcs = [], l = arguments.length, arg;
			for(var i = 0; i < l; i++)
				{
				arg = arguments[i];
				if(typeof arg == "function") funcs.push(arg);
				}
		
			//add final route to routes object:
			this.routes.push({route: (no_route? false : route), params:vars, functions:funcs});
			},
		
		go: function(str)
			{
			var current = -1, route_followed = false, routes = this.routes;
			
			var pivot = str.indexOf("?");
			var querystring = "", url = str;
			if(pivot >= 0)
				{
				querystring = str.slice(pivot+1);
				url = str.slice(0,pivot);
				}
			
			var split_str = str.split("?");
			var req = {
				location: str,
				args: Array.prototype.slice.call(arguments, 1),
				url: url,
				query: parseQuery(querystring),
				querystring: querystring
				};

			function next(error)
				{
				//no more routes to try, so finish:
				if(++current >= routes.length) return;
			
				//check that route matches:
				var route = routes[current];
			
				//if there is a route to match:
				var match = [];
				if(route.route)
					{
					//only follow first matching route:
					if(route_followed) return next();
					match = req.url.match(new RegExp(route.route));
					if(!match) return next();
					else route_followed = true;
					}
			
				//label parameters if necessary:
				var ps = route.params, ps_length = ps.length;
				if(ps_length)
					{
					req.params = {};
					for(var i = 0; i < ps_length; i++)
						req.params[ps[i]] = match[i+1];
					}
				else req.params = match.slice(1);
			
				var inner = -1, inner_funcs = route.functions, inner_l = inner_funcs.length;
				//run funcs:
				function runInner(error)
					{
					//if no more inner funcs, go to next outer:
					if(++inner >= inner_l) return next(error);
					if(typeof error != "undefined") req.error = error;
					//catch error, log and continue:
					try{ inner_funcs[inner](req, runInner); }
					catch(e) { console.error(e.stack); runInner(e); }
					}
				runInner(error);
				}
			next();
			}
		}
	}
