+++
title = "A React Developer Tries Svelte"
description = "After spending the last couple of years working primarily with React on the frontend, I was introduced to Svelte and was compelled to give it a go. Here are some of my thoughts."
date = 2020-10-31
+++

React and Svelte are both frameworks that are in charge of keeping your HTML in sync with your data, and both make it easy to create highly interactive web pages.

I've been using React professionally for around 2 years now. I've been a huge fan of the functional style that React takes, and it felt like a huge step forwards from my AngularJS 1 days. A few weeks ago I was linked to an [excellent talk][svelte-talk] called "Rethinking Reactivity" by Rich Harris, the creator of Svelte. The talk left me feeling like Svelte might be the next step forwards after React, and compelled me to give it a try myself. Since then, I've created a couple of small Svelte based interfaces, and it's time to reflect on how I feel about Svelte so far, and contrast it with React.

This won't be an exhaustive look at the differences between React and Svelte; the focus will be around those differences that particularly appeal to me (or not).


# Initial setup

I love how easy it is to get started with Svelte; for me it basically consists of these steps:

- Run the suggested command from the website: `npx degit sveltejs/template my-svelte-project`.
- Then, run `node ./scripts/setupTypeScript.js` to switch the project to using TypeScript.
- Edit the `tsconfig.json` file to make it stricter (notably: `compilerOptions.strict = true`).

From that, I have a very minimal Svelte app that includes:

- Scoped CSS (and warnings about unused CSS).
- TypeScript support.
- A decent looking build/watch system.
- Very few other dependencies; just enough to get going with.

The "starter" templates I find for React don't tend to align so well with my personal preferences. They tend to ship with a bunch of dependencies I don't care for in the beginning, but also miss some things that I do care about. I usually end up installing Parcel and manually adding TypeScript and setting up scoped CSS before I can begin. That all said, more recent versions of `create-react-app` look like they will provide what I want out of the box nowadays, so I'll need to go back and look at that again too. I've been put off by the complexity of Webpack in the past, but I expect it has a good ecosystem around it at least.

In any case, I like that Svelte provides an officially endorsed starting point that's almost perfect for me from the get go, and I hope this remains the case going forwards. While for larger apps I can justify spending the time to get the build system set up just right, for small apps it's not something I want to give a second thought to if I can avoid it.


# Speed and Bundle Size

The most significant difference and thing that compelled me to give Svelte a go in the first place is that Svelte will generate code at compile-time to handle interactivity, whereas React will use virtual DOM diffing at runtime to achieve the same.

Svelte promises much smaller bundles and faster code as a result of pushing a bunch of this work to the compile stage and so not needing to ship with anything like a virtual DOM implementation. I expect that for larger apps, the relative cost of bundling something larger like React becomes smaller. I also wonder whether Svelte bundle sizes grow more rapidly as you add components, as opposed to the one off cost you pay for bundling React.

I could see myself using Svelte for small interactive components in an otherwise static website, or for building a component library with Svelte alongside adapters to make it work with React, Vue etc. The fact that the output from Svelte is so low in overhead (I imagine that writing Svelte like components in raw JS would lead to not-much-smaller-if-at-all bundle sizes) makes this a compelling option. I just wouldn't think to use React in quite the same way as it's size makes it a bigger commitment.

All in all, the small bundle size is particularly appealing when writing small applications, or adding small bits of interactivity to otherwise static pages. The speedup Svelte claims from this is welcome anywhere!

# Compile-time vs Runtime Reactivity

## React

In React, the default behaviour is that any changes to the state, context or props of a component will lead to the component function and any child component functions being re-run. Each of these components will then produce a new description of what the part of the DOM it is responsible for should look like.

React will work out the difference between these and the previous descriptions, and then go away and mutate the _actual_ DOM based on those differences to keep the DOM in sync. The theory is that mutating the DOM is expensive, and so React will try to mutate only what is necessary.

Here's an example in code:

```tsx
import * as React from "react"
import { useEffect, useState, useRef } from "react"

// This will return the number of times that
// the component function has been called:
function useRenderCount() {
  const countRef = useRef(1)
  useEffect(() => { countRef.current += 1 })
  return countRef.current
}

// This is the entry-point to our react application:
const App: React.FC = () => {
  const [count, setCount] = useState(0)
  const rerenders = useRenderCount()
  return (
    <div>
      Parent re-render count: {rerenders}
      <button onClick={() => setCount(c => c+1)}>Increment</button>
      <Child/>
    </div>
  )
};

// If we click the button in the App component,
// we'll see the re-render count increase for this, too.
// If we click the button in the Child component,
// the Child and GrandChild will re-run.
const Child: React.FC = (props) => {
  const [count, setCount] = useState(0)
  const rerenders = useRenderCount()
  return (
    <div>
      <div>Child re-render count: {rerenders}</div>
      <button onClick={() => setCount(c => c+1)}>Increment</button>
      <GrandChild/>
    </div>
  )
}

// Whenever Child reruns, this will run as well.
const GrandChild: React.FC = (props) => {
  const rerenders = useRenderCount()
  return (
    <div>GrandChild re-render count: {rerenders}</div>
  )
}
```

Because React runs the entire component tree beneath some change every time, we will eventually start wanting to optimise certain components to reduce the amount of work that's done. This can involve surrounding expensive computations within a component in `useMemo` or `useCallback`, or surrounding the entire component in `React.memo`. In short, React runs _everything_ by default, and we work to reduce the amount of things that need to be re-run when optimising our app.

## Svelte

Svelte starts from the other end of the spectrum. At compile-time, Svelte will generate the code needed to mutate precisely what it needs to each time something changes. Because the code to handle the reactivity is generated at compile time, Svelte doesn't rely on doing DOM diffing at runtime, and doesn't need to ship the supporting code to perform it. This leads to a reduction in bundle size (no need for a Virtual DOM implementation) and in speed (no runtime diffing, just targeted mutations).

So, in Svelte, instead of annotating the things you wish to avoid re-running, you end up annotating the things that you _do_ want to re-run:
- Values/blocks/statements prefixed with `$:` will be re-run if any of the variables mentioned in them are known to have been changed.
- Any assignment to a value (eg `fullName = firstName + lastName`) is instrumented at compile-time so that Svelte knows to update anything that depends on the value assigned to (`fullName` in this case).

Here's a very basic Svelte example, which is somewhat like the React example but without a `useRenderCount` (which doesn't really make sense in Svelte land):

**App.svelte:**
```html
<script>
	import Child from './Child.svelte'
	let count = 0
</script>

<div>
	<div>{count}</div>
	<button on:click={() => { count += 1 }}>Increment</button>
	<Child parentCount={count}/>
</div>
```

**Child.svelte:**
```html
<script lang="ts">
    export let parentCount = 0
    let count = 0
    $: totalCount = count + parentCount
</script>

<div>Count: {count}, Parent Count: {parentCount}, Total: {totalCount}</div>
<button on:click={() => { count += 1 }}>Increment</button>
```

Here, calls to `count += 1` are instrumented at compile time to update anything depending on `count`, and as a result of using `$:`, `totalCount` will be updated each time `count` or `parentCount` are.

The upside of the Svelte approach is performance; updates are performed with surgical precision. It's a bit like everything being automatically surrounded in `useMemo` and `useCallback` and so on for you, but even better.

The downside is that you must remember the rules to make things reactive. Take this example:

```html
<script lang="ts">
    export let parentCount = 0
    let count = 0
    function getTotalCount() {
        return count + parentCount
    }
    $: totalCount = getTotalCount()
</script>

<div>Count: {count}, Parent Count: {parentCount}, Total: {totalCount}</div>
<button on:click={() => { count += 1 }}>Increment</button>
```

Here, Svelte no longer updates `totalCount`, because `$: totalCount = getTotalCount()` no longer mentions `count` or `parentCount`, and so Svelte doesn't know that `totalCount` should be updated when they are. It's a bit like forgetting to include a dependency in a call to `useMemo` in React; dependencies have to be mentioned in some way for either framework to know that an expression needs re-running.

I think that this requirement to explicitly assign to values, or use `$:`, will lead to slightly different APIs becoming preferable for Svelte functions. For instance, anything that updates a variable in a svelte component will probably want to take a callback which actually does the assigning, so that Svelte can instrument it properly. Some functions might end up taking a list of dummy dependencies as arguments, like `getTotalCount(...deps: any[])`, so that I can manually tell Svelte when to re-run certain functions by way of passing the dependencies in. I'm not sure yet; it'll take some time to arrive at a good approach to certain cases I think.

In summary, React re-runs things very aggressively, and you need to optimise the code by adding explicit `useMemo`'s and such. Svelte re-runs only what it thinks needs to re-run, so it's much closer to optimal off the bat, but you sometimes need to be explicit to make sure it knows when to re-run things.


# Hooks

React uses hooks to, in part, bridge the gap between the component lifecycle and the lifecycle of external code. Here's an example of this:

```tsx
import { subscribeToThing } from '../sub'

function LastMessage (props) {

    const [lastMessage, setLastMessage] = useState('')

    useEffect(() => {
        return subscribeToThing(props.subscriptionId, setLastMessage)
    }, [props.subscriptionId])

    return (
        <div>{lastMessage}</div>
    )
}
```

In this, our `LastMessage` component subscribes to messages from some place, and sets the `lastMessage` to the latest message it's given back. We want it to re-subscribe to new messages each time the `subscriptionId` property is updated, so we mark that as a dependency of the `useEffect`. The subscription does not update whenever the component re-renders; it has its own lifecycle.

When I first started using Svelte, I wondered where the alternate to hooks was. However, most hooks just aren't needed. `useCallback` and `useMemo` are unnecessary since things only re-run as needed anyway. `useRef` isn't needed for similar reasons. `useState` is replaced with plain variables and compile time magic. `useEffect` is replaced by using `$:` to run code when dependencies change, and by `onMount`, `onDestroy`, `beforeUpdate` and `afterUpdate` which allow us to hook into the Svelte component lifecycle if needed.

Not needing hooks is great, it turns out. Hooks [come with rules][hook-rules] which limit how they can be used, and can be tricky to compose, but plain old functions can be composed and reused any which way you like. Certain Svelte functions do need to be called synchronously with the component being initialised however. I wish that those followed a naming convention like hooks do, so that it is obvious which functions have these restrictions. I might stick to pre-pending `use` to such functions, or find another more suitable word.

The above component in Svelte, for reference, might look something like this:

```html
<script lang="ts">
    import { onDestroy } from 'svelte'
    import { subscribeToThing } from '../sub'
    export let subscriptionId: string

    let doSubscribeToThing = useSubscription()
    let lastMessage = ''

    // This will be called every time subscriptionId changes, leading
    // to a re-subscription.
    $: doSubscribeToThing(subscriptionId, msg => { lastMessage = msg })

    // Be a little stateful; unsubscribe from the last subscription
    // before re-subscribing, and when the component is destroyed.
    // This covers what useEffect does for us.
    function useSubscription() {
        let unsub: () => void = () => null
        onDestroy(() => unsub())
        return (subId: string, cb: (msg: string) => void) => {
            unsub()
            unsub = subscribeToThing(subId, cb)
        }
    }
</script>

<div>{lastMessage}</div>
```

This does help to highlight how well thought through the `useEffect` API is (I think, at least); in Svelte we are perfectly able to emulate the same behaviour, but we must manually handle both resubscribing and cleanup as needed. `useEffect` takes care of all of this for us.

Perhaps it's a good idea in Svelte to draw inspiration from `useEffect`. This might lead us to something like:

```typescript
let subscribeEffect = useEffect()
let lastMessage = ''

// This reruns whenever any variable mentioned in it changes, so it's
// a bit like useEffect without the explicit dependencies:
$: subscribeEffect(() => {
    return subscribeToThing(subscriptionId, msg => { lastMessage = msg })
})

// This could live somewhere generic. It needs to be called in sync with
// the component being created, but unlike hooks it can be used in loops
// and conditionals and such.
function useEffect() {
    let unsub: () => void = () => null
    onDestroy(() => unsub())
    return (cb: () => () => void) => {
        unsub()
        const cbRes = cb()
        unsub = typeof cbRes === "function"
            ? cbRes
            : () => null
    }
}
```

I'll have to think about whether this is a good idea or not, but needless to say, I think that Svelte has the ability to emulate the sorts of things we can do with hooks in React (and perhaps be even better at it).


# Component Layout

In Svelte, the HTML(-like) description, code, and styles live in a single file. An empty file is a valid Svelte component, so it's pretty light weight. Since this setup comes out of the box I can get up and running very quickly, which is lovely. This is a valid Svelte component:

```html
<h1>Hello!</h1>
```

As is this:

```html
<script lang="ts">
  export let name: string
</script>

<h1>Hello, {name}</h1>

<style>
  h1 { font-size: 20px; }
</style>
```

A typical React setup will separate styles from components. That said, React ultimately offers more flexibility here; you can configure plugins to give you inline CSS, and you can define multiple components in a single file. Code that is global to components is also natural and easy to add; you can achieve the same in Svelte but it's not as immediately obvious how.

In React, I'd often incrementally break down a component into smaller components in the same file while I was iterating on it, and later I might decide to move those out to separate files. I miss this a little in Svelte; components in Svelte _are_ lightweight, but having to make a separate file and import it each time I want to break anything down can be more of a hinderance than defining a new function. I feel like I've tended towards slightly larger Svelte components as a result. (This has nothing on my Angular 1 days, when an awful lot of boilerplate went into defining a new component when compared to either of these frameworks).


# TypeScript

Both React and Svelte support TypeScript, and nowadays getting that support out of the box is easy for both.

That said, Svelte seems to have slightly weaker TypeScript support than React, though it generally holds up much better than I was expecting it would. As with React, component interfaces and code inside the HTML templates is all type-checked. Template syntax in Svelte like `{#if foo}` and `{#each item in items}` is also type-checked, which I was pleasantly surprised by.

## Component Props

One area that Svelte falls down a little is defining component properties. In React, I can give complex types to component properties, and make use of generics as much as I like, for example this is perfectly fine:

```tsx
type Props<T> = {
    data: T[]
} | {
    datum: T
}

function MyComponent<T>(props: Props<T>) { ... }

// Usage:
function App() {
  return (
    <div>
      <MyComponent data={[1,2,3]}/>
      <MyComponent datum={1}/>
    </div>
  )
}
```

Being able to "or" properties can be useful when building components like inputs, which only accept certain properties based on the value or presence of others. `<MyInput type='range' min={0} max={100}/>` and `<MyInput type='number' value={10}/>` can be well typed.

I've made use generic props in components a little too, for instance when building a graph or table component that doesn't care exactly what shape the data is, as long as it's told how to get at the values it needs for each column/axis (eg `<Graph data={values} x={item => item.date} y={item => item.value}>` or `<Table data={data} columns={[{ title: 'Foo', value: item => item.foo }]}`).

Both of these cases aren't super common, but highlight a place where Svelte falls down in its TypeScript support and are things I think I'd run into occasionally when working on larger projects. I expect I could mostly come up with different APIs to work around it.

It's worth noting that to get well typed component props _at all_ in Svelte, I had to set `compilerOptions.strict` to true in my `tsconfig.json`. This was something I stumbled over initially and was somewhat off-putting until I found a way around it. It's something I'd like to have seen documented more prominently.

## Custom Events

Svelte has an approach whereby you can dispatch custom events for parent components to subscribe to. This is taken from the Svelte docs for instance:

```html
<script lang="ts">
	import { createEventDispatcher } from 'svelte'

	const dispatch = createEventDispatcher()

	function sayHello() {
		dispatch('message', {
			text: 'Hello!'
		});
	}
</script>

<button on:click={sayHello}>
	Click to say hello
</button>
```

A parent component could then subscribe to `on:message` to receive these events.

I was expecting to point out that this couldn't be well typed either, but it turns out that I was wrong! By default, the `message` event dispatched isn't super well typed (the event value defaults to `any` which makes me a bit sad; I'd rather it was `unknown`). However, I can change the dispatch line to:

```typescript
const dispatch = createEventDispatcher<{ message: { text: string } }>()
```

With this, the parent component actually ends up with a well typed interface to subscribe to these messages!

Support doesn't seem to be as good when forwarding events however (eg using `<button on:click>` in a child component to forward `on:click` messages to the parent); I think you'd have to manually `createEventDispatcher` with the correct type and emit messages using that to be well typed here.

So far, I've just defaulted to passing functions down to be called instead of using this message interface, since it's well typed by default and it's what I'm used to doing in React. I'll have to explore this area a little further to decide on how much to use the message passing bits of Svelte.


# Two Way Bindings

React is pretty strongly in the 'one way bindings' camp; you pass functions down if you want to get any data back from a child component. Svelte provides more support for two way bindings via the `bind` directive. Binding allows child components to directly mutate values in parent components, as well as parent components mutating those same values to effect a change in the child component. In Svelte, the built in form components like `input` and `select` all have an interface which relies on binding by default, and you can also `bind` to custom properties instead of just passing values down to them.

I've so far avoided using this on custom components. I've run into plenty of issues using two way bindings from my time with Angular, and prefer the explicit approach React takes. That said, I can't deny that binding can reduce boilerplate, and so with time I expect I'll find a happy medium when using Svelte.

One thing that's lovely about Svelte in this area is that it provides built-in bindings for things like obtaining the width and height of an element, which is known to be annoying to do and so is a nice touch to have that out of the box.

# Template Syntax

React uses JSX/TSX as the primary means by which a component can describe what it wants the DOM to look like. This de-sugars to function calls (which themselves desugar to objects). You're able to intersperse code and JSX, and in fact that's how you can conditionally show or hide components, or output arrays of components:

```tsx
function MyComponent(props) {
  return (
    <div>
      { props.showTitle && <h1>Hello!</h1> }
      <ul>
        {
          items.map(item => (
            <li>{item.name}</li>
          ))
        }
      </ul>
    </div>
  )
}
```

Essentially, you can nest code within `{}` blocks, and from these blocks you can return anything that can be rendered, from primitive types to other components or even arrays of other components.

In general, I have a lot of power over exactly what I display and how in React. For instance, I can do things like:

```tsx
const Thing
  = foo == 'a' ? <Foo bar='wibble'/>
  : foo == 'b' ? <Lark/>
  : null

return (
  <div>
    <Thing/>
  </div>
)
```

Svelte's template syntax is similar in some ways, also using `{}` to interpolate code and such into templates. However, you can't do things like return components from `{}` blocks, and so conditionals and looping need to be handled separately. The above might look more like this in Svelte:

```html
<div>
  {#if props.showTitle}
    <h1>Hello!</h1>
  {/if}
  <ul>
    {#each item of items}
      <li>{item.name}</li>
    {/each}
  </ul>
</div>
```

Svelte covers the common cases with what I regard as a nicer syntax, but it lacks the flexibility to easily compose and combine fragments of this DOM description. To regain some of this flexibility it has to handle individual use cases with special syntax or handlers, such as `<svelte:component this={SomeComponent}/>` to dynamically render different components, or `<svelte:self/>` to reference the component we're currently defining.

A couple of the special components that Svelte provides are quite handy. `<svelte:head/>` allows you to insert elements into the document head, and things like `<svelte:body/>` give you an easy way to attach event handlers to certain elements outside of your component in a safe way. You'd probably end up doing DOM manipulation to achieve either of those cases in React.


# Batteries Included

Svelte has a bunch of bigger and smaller quality of life features that I quite like, for instance:
- A build setup I am happy with out of the box, including TypeScript.
- Scoped CSS in the same file, with warnings about unused CSS.
- A decent looking built-in store implementation, as opposed to needing to decide which of the hundred React store implementations to use.
- Good looking support for animations and transitions out of the box (I've not played much with these yet, but it's just one less library I'll have to read about and install when the time inevitably comes).
- A shorthand for conditionally adding/removing classes on components (in React I'd `npm i classnames` to handle this sort of thing, but in Svelte it's more like `class:active={isActive}`).
- Helper components like `<svelte:head/>` to make it easy to add code and events as needed by components.
- Various smaller bits and pieces.

All of this makes it very easy to get going with a Svelte project, and avoids the sometimes overwhelming amount of thought that needs to be put into deciding on the libraries to use for stores, animations, scoping CSS and so on. Mostly it's a "take it or leave it" case with Svelte; you don't pay for what you don't use, and so Svelte can include a bunch of useful stuff without concern. Svelte has a clear focus on productivity, and it shows.

If you have strong opinions over what libraries/approach you want to take in certain areas, you might prefer React in this area. Personally I find it easy to go down the rabbit hole for far too long trying to decide what the best libraries are for X and Y thing, so having some opinionated choices built in takes a burden off my shoulders.


# Slots / Passing Components to Children

In React, components can be passed children like so:

```tsx
return (
  <MyComponent>
    <div>Some child</div>
  </MyComponent>
)
```

Inside `MyComponent` the children are then available as `props.children`. You can also pass components to any arbitrary props, if you want to "name" them:

```tsx
<MyComponent
  title={<h1>Hello!</h1>}
  body={<div>Some body</div>}
/>
```

Since components are first class citizens throughout, you can compose and pass them around as you like.

In Svelte, components don't feel first class in the same way. We need to use the special `<svelte:component>` to render dynamic components, for example:

```html
<!-- TakesChild.svelte -->
<script lang="ts">
  import type { SvelteComponent } from "svelte"
  export let child: typeof SvelteComponent
</script>

<svelte:component this={child}/>
```

This can be passed some component by its parent, for instance:

```html
<script lang="ts">
  import Child from './Child.svelte'
</script>

<TakesChild child={Child}/>
```

This is a little ugly, and doesn't obviously work if you want to pass some compound thing like `<div>Hi</div>`. To pass these sorts of things in Svelte, you'll need to use the special `<slot/>` thing:

```html
<!-- TakesChild.svelte -->
<slot/>
```

And then the parent can do:

```html
<script lang="ts">
  import Child from './Child.svelte'
</script>

<TakesChild>
  <Child/>
  <span>arbitrary other stuff here</span>
</TakesChild>
```

Slots can be named too, to allow multiple things to be passed down in this way.

All in all, Svelte has to have special syntax to pull off certain things, whereas React gives you the full flexibility of arbitrary code in most cases. In general, I assume this is because Svelte has to perform compile-time magic to avoid runtime DOM diffing bits, and so that limits the runtime flexibility that you have.


# Ecosystem and maturity

Svelte is obviously less mature than React, and TypeScript support in Svelte 3 has only been around for a matter of months at the time of writing. The ecosystem will be less mature, as will the level of support and such you'll be able to find from various posts and articles.

That all said, it's pretty easy to embed "plain" JavaScript libraries like D3 into either Svelte or React, and so I'm not too concerned about the lack of Svelte vs React specific bits and pieces. for larger projects though, this might lead to a slowdown as you need to implement things in Svelte that libraries exist for in React. I haven't "shopped around" for libraries enough in Svelte to get a feeling for hw big this difference really is, though.


# Conclusion

So far, I'm generally really enjoying Svelte, but I've only used it for fairly small projects to date. The major advantages it has over React for me are:

- It's really quick to get going with; batteries included and other niceties make it a breeze to start hacking on a Svelte project.
- It's fast and compiles down to tiny bundles.
- No need for things like hooks, and hook dependencies are essentially automatic.

The disadvantages for me are:

- The TypeScript support is a little weaker, especially when it comes to component interfaces.
- There's a loss of flexibility/composition, partly down to the separation of HTML and code, and probably partly down to the compile time approach taken. A bunch of special cases are needed to handle things that React can handle without special constructs.
- If you don't follow the rules (`foo = bar` and `$:` lines mentioning all dependencies), you'll end up with things that don't update as you expect in Svelte. I've run into this a couple of times, but it's been easy enough to figure out and fix so far.
- One component per file can slightly discourage breaking up Svelte components (though I like CSS being included!).

In general, I've been enjoying Svelte, and it feels like it has some clear advantages for smaller projects. I feel like for larger projects I might bump into the flexibility and type checker downsides a little more, but I'm a sucker for the speed/size benefits and so I'll certainly give Svelte some consideration in any case.

Overall, the approach that Svelte takes is pretty compelling. Doing more at compile time feels like a solid step forwards, and something I could imagine other major frameworks looking more towards in the future.


[svelte-talk]: https://www.youtube.com/watch?v=AdNJ3fydeao
[hook-rules]: https://reactjs.org/docs/hooks-rules.html
