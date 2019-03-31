+++
title = "Notes on Kubernetes"
description = "Some notes I wrote up on Kubernetes from having attended the beginner and intermediate training courses hosted by JetStack at the Google building in London."
date = 2019-03-31
[extra]
created = "2019-03-17"
+++

*Docker* is a platform for building *images*, which contain everything your application needs to run. These images can be quickly spun up into *containers*, which can be thought of as lightweight VMs. They share the underlying OS kernel where possible, and isolate themselves from other processes using linux kernel features rather than full fledged VMs.

*Kubernetes* is a container orchestration platform which helps coordinate the running of these Docker images. In plain english, you use Kubernetes by describing what and how you want various images to run, and Kubernetes takes care of making sure that what *is* running lines up with the description it's been given.

Kubernetes is particularly good at running stateless applications, since it can scale up and down the number of things you want running as well as the number of machines to run these on, perform rolling updates, and direct traffic to groups of containers quite easily. It's possible to run stateful applications as well, though more care needs to be taken in doing so.

# Prerequisites

If you'd like to follow along with the examples, you should install [kubectl][kubectl] and set up a Kubernetes cluster somewhere. The easiest way to do this is to install and run [minikube][minikube], which starts up a small single-node cluster on your machine that you can play with. Alternately you can spin up a cluster on Google Cloud or some other provider, and everything should work in the same way.

These notes and examples are based on using `kubectl` version 1.13 and `minikube` version 0.35.

# Overview

Life starts with a *Kubernetes Cluster*. This consists of a set of *nodes* (physical machines, for instance) that actually run your applications, and one or more *masters* that coordinate the running of them. You can create your own cluster, or use clusters provided by services like Google Cloud, Amazon EKS or various others. In each case, the way in which you interact with it is the same.

![A diagram showing the rough layout of a Kubernetes cluster][kube-cluster]

Each master has a copy of a distributed key-value store called *etcd*, which is where configuration describing what should be running lives. You control Kubernetes by editing this configuration to describe how you want your applications to run. Things watching that configuration on the master then go forth to make reality match it.

The configuration files you write to describe your kubernetes environment are written in YAML, which is a [superset of JSON][json2yaml]. Most of the time, you'll write a bunch of these files and then push them to Kubernetes via the API that it provides. This is normally done via the `kubectl` CLI tool which just provides an interface to the API. You can manually talk to the API via its HTTP interface using JSON as well.

At a basic level, a running Kubernetes cluster consists of:

- *Pods*, each of which contains one or more Docker containers.
- *Services*, which describe how these Pods can communicate with each other and the outside world.
- *Controllers*, that we tend to create instead of directly working with Pods. These take care of things like scaling the number of Pods up and down and transitioning between old and new versions of Pods as we update their configuration.
- *ConfigMaps* and *Secrets*, which are just configuration files that store variables you want to provide to the containers inside our Pods (as either files or environment variables).
- *PersistentVolumes* and *PersistentVolumeClaims*, which allow you to define persistent storage and make it available to our containers.

I'll look at each of these things in turn, stopping short of persistent storage and leaving that as an exercise for the reader, but I hope by then you'll have gained a decent intuition for how Kubernetes works.

# Pods

A *Pod* is the basic building block in kubernetes that describes something you want to run (for example, a database or a web server or anything else). A Pod can contain one or more *containers*, which are just running instances of Docker images. Pods often contain just one container; by keeping Pods small, each part of your application can be scaled up and down, updated, and reconfigured independently of each other.

In short, if you want to get some application running in Kubernetes, you'll typically take the following steps:

* Write a `Dockerfile` for your application. This describes how to package it up into a docker image that contains everything necessary to run it.
* Push the image to a Docker container library on the internet that your kubernetes cluster can access (this is reminiscent of pushing code to github for instance; you can also `pull` Docker images from a container library to run them locally). This can be a private library in Google Cloud, for instance, or a public one like [Docker Hub][dockerhub].
* When describing a Pod you'd like to run in Kubernetes, you can then point to the image you'd like it to run. Kubernetes will then be able to download the image and spin it up whenever it needs to.

Here is the configuration for a Pod that simply runs an echo server on port `3000` (by default):

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: echo-server
  labels:
    app: echo-server
spec:
  containers:
  - name: echo-server-container
    image: kennship/http-echo
```

Here, we give the Pod a name of "echo-server" and give it a single key-value label of `app: echo-server`. We could add labels specifying the version of our app and so forth as well if we like. We can use labels to select matching Pods in various scenarios, so they are really useful. Finally, we provide a spec describing the containers we want to run. The image name here (`kennship/http-echo`) refers to an image on Docker Hub by the same name that we'd like to run.

If we save this to a file, eg `echo-server.yaml`, we can run it in our Kubernetes cluster by running `kubectl apply -f echo-server.yaml`. This command sends whatever configuration file (or folder) you point it at to Kubernetes, which then updates the cluster to reflect it.

`kubectl get pods` should now list a single Pod, `echo-server`, as running. `kubectl get pods -o wide` shows some extra information, including the Pods' cluster IP addresses.

To test this echo server, we can spin up an interactive `busybox` prompt in another Pod with the following:

```
kubectl run -it busybox --image=busybox --rm --generator=run-pod/v1
```

`--image=busybox` specifies the Docker image we want to run. `-it` provides `stdin`/`stdout` so that you can interact with the running application (busybox in this case). `--rm` removes the Pod when we are done with it. `--generator` specifies the type of thing this command should create (here, just a Pod, but we'll learn about Deployments and such soon). Finally, we have to give the Pod that this command creates a name, in this case "busybox".

Running this should give us a prompt inside a Pod running `busybox`, so we're now inside our Kubernetes cluster! Now, if we get the IP address of the `echo-server` Pod using `kubectl get pods -o wide` in another terminal (Pod IP addresses are internal to the cluster), we can communicate with the `echo-server` Pod from our `busybox` prompt by typing something like `wget [echo-server-ip-address]:3000 -O -`. As the name states, this will echo back some information about the request you just made.

Some other useful commands in the area:

`kubectl describe pod echo-server` describes the `echo-server` Pod, including showing a recent event log. This is a handy command if you want to debug why a Pod is not running properly.

`kubectl logs echo-server` shows log output from the echo-server.

`kubectl get pod echo-server -o yaml` outputs the current yaml configuration stored for the `echo-server` Pod. You'll see loads more than our tiny config, including various defaults and current Pod status information, which is all written to the configuration in Kubernetes as the Pod runs.

`kubectl exec -it echo-server bash` runs a command inside the `echo-server` Pod's only container (you can specify the container to run the command in as well if need be). In this case, we specify `-it` to get an interactive terminal, and run `bash` so that we can snoop around.

`kubectl attach -it busybox`, while busybox is running using the above command, will attach another terminal to the running busybox process. You can generally use this command to attach yourself to whatever is currently running in some Pod.

`kubectl delete pod echo-server` deletes the `echo-server` Pod.

There's loads more that you can configure about Pods and the containers that live inside them; you can peruse the full API [here][api-pods]. Here are some interesting ones for Pods:

- `spec.volumes`: Make volumes available to the Pod that can then be mounted into the containers inside it.
- `spec.affinity`: Decide which other Pods or Nodes this Pod will be scheduled near to or away from. Sub properties include:
  - `podAffinity`/`podAntiAffinity`: Describe which other Pods you want the Pod to be spun up next to or to avoid. You use labels to select other Pods, and a `topologyKey` to select where to schedule this Pod based on the locations of the Pods matched. Using this, you can make sure Pods are spun up on the same machine, or just the same geographic Region, or anything else based on the labels you give your Nodes.
  - `nodeAffinity`: Schedule the Pod onto Nodes matching the labels selected.
- `spec.tolerations`: Nodes can be tainted to avoid things being scheduled onto them (for example, maybe you have a high spec Node that you only want specific jobs to run on). `tolerations` allow a Pod to be scheduled onto one of these tainted Nodes.
- `spec.initContainer`: If we define this container, the Pod won't be marked as ready until this container has successfully completed.

And for the containers we want to run (`spec.containers` in the Pod configuration):

- `resources`: Define the resources (CPU/memory) that you expect the container, and limits before the container will be throttled/killed. The resources are defined in terms of the CPU/memory available on the Node the Pod is running on.
  - `requests`: Define what you expect the Pod to consume ordinarily. The Kubernetes scheduler uses this information to decide on which Node it can fit the Pod. You should almost always use this.
  - `limits`: Define at which point to throttle the container or kill it. This is useful to kill containers that get stuck into infinite loops or leak memory for instance.
- `volumeMounts`: For the `volumes` that we make available in the Pod spec, we can mount them into specific places in the container.
- `env`: Set environment variables for the container.
- `envFrom`: Inject environment variables from a `configMap` or `secretMap` (these are things you can write YAML config to create, and contain lists of useful variables and such that you can share across containers).
- `command`/`args`: Run a command when the container starts up, or provide args to override the command that was already configured to run from the Docker image.
- `readinessProbe`/`livenessProbe`: Define when the container is ready to start receiving traffic and whether it's still alive. These can be a command that needs to run on the container, or an HTTP GET request to perform against it, for instance. A Pod is only marked as ready once all of the containers in it are. The container is restarted (not the Pod) if `livenessProbe` fails.

# Namespaces

If we have lots of things we'd like to run in our Kubernetes cluster, we can put things into namespaces. Most `kubectl` commands allow you to add `-n some-namespace` as a flag to run inside that namespace. the `metadata.namespace` setting in configuration files sets the namespace that things will be deployed into.

When something is in a namespace, it won't show up when you run commands like `kubectl get resource thing` unless you use the`-n` flag as well to set the namespace. You can however add `--all-namespaces` to show things everywhere.

To create a namespace, you can either run `kubectl create namespace my-namespace-name`, or add it via config like anything else, for example:

```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: my-namespace-name
```

As with other objects in Kubernetes, we can view our namespaces by running `kubectl get namespaces`.

# Controllers

One of the issues with creating Pods manually is that if they are terminated for whatever reason, they won't be recreated. Your Pod configuration is like a cookie-cutter that cuts out some cookies and then has nothing more to do with them.

Controllers in Kubernetes control some aspects of how Pods are created, restarted, and updated. To use a controller, you'll define what a Pod looks like in the same way as above, but provide other configuration describing things like how many instance of that Pod should be running. A common controller is a *Deployment*, which coordinates the creation of *ReplicaSets*, whose job it is to make sure a certain number of copies of some Pod are always running.

While you can create ReplicaSets directly, Deployments are more powerful, and almost always what you want to be using. That said, since they build on each other, it's a good idea to look at how ReplicaSets work first.

## ReplicaSet

The job of a ReplicaSet is to keep track of how many of some Pod are running (given some `selector` which matches against Pod labels). if that count goes above the number of `replicas` we've configured, the ReplicaSet will delete Pods. If the number of Pods that the ReplicaSet is responsible for dips below the `replicas` count, the ReplicaSet will create new Pods based on the Pod template provided.

We can create and apply a ReplicaSet for our `echo-server` as follows, to ensure that 3 copies of it will always be running:

```yaml
apiVersion: apps/v1
kind: ReplicaSet
metadata:
  name: echo-server
  labels:
    app: echo-server
spec:
  replicas: 3
  selector:
    matchLabels:
      app: echo-server
  template:
    metadata:
      labels:
        app: echo-server
    spec:
      containers:
      - name: echo-server-container
        image: kennship/http-echo
```

The `spec.template` field is a Pod spec describing the Pod that the ReplicaSet should create when necessary. `spec.selector` tells the ReplicaSet what Pods are its responsibility (and so it should line up with `spec.template.metadata.labels` so that the Pods created by the ReplicaSet are its responsibility).

Once we have saved and applied this, `kubectl get replicasets` will now show this new ReplicaSet, along with the state of the underlying Pods. If we run `kubectl get pods`, we'll see several Pods of the above spec, each with a random hash appended to their name so that they have unique identifiers.

If we left the original `echo-server` pod around from earlier, that will become the responsibility of the ReplicaSet since it matches the selector, so the ReplicaSet will create two new Pods with a random hash, to bring the count up to 3.

If we `kubectl delete` one of these Pods, we'll see that a new one is spun up in its place (the ReplicaSet is doing its job!). You'll need to delete the ReplicaSet in order to permanently delete the associated Pods.

If we edit the number of `spec.replicas` in our config and `kubectl apply` it again, the ReplicaSet will spin up or destroy Pods as necessary to make reality match this new configuration.

`kubectl edit replicaset echo-server` is another way to edit the details of our ReplicaSet (or indeed any other object in Kubernetes), but it's generally not recommended to edit in place like this as your local config files will then be out of sync with the actual state of the system.

If we edit the details in `spec.template` (the Pod spec) this won't change any of the existing Pods. Instead, new Pods that are spun up will be based on the new template. This can lead to your system being in an inconsistent state, with different versions of the Pod lying around. *Deployments* allow us to update Pods in a more controlled way.

## Deployment

The job of a Deployment is to manage ReplicaSets. Each time we make a change to the Pod spec (for instance, we update the Docker image our Pods are running), the Deployment will spin down the existing ReplicaSet and spin up a new one according to the update strategy we have provided (by default, it'll perform a rolling update, gradually shrinking the size of the old ReplicaSet and increasing the size of a new one).

When you create a Deployment, you'll find that a corresponding ReplicaSet has been created (with a random hash appended to it as it's managed by the Deployment), and also the corresponding Pods have been created (now with *two* random hashes appended to their names).

Let's `kubectl delete replicaset echo-server` and instead create a Deployment for it that looks like the following:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: echo-server
  labels:
    app: echo-server
spec:
  replicas: 3
  selector:
    matchLabels:
      app: echo-server
  template:
    metadata:
      labels:
        app: echo-server
    spec:
      containers:
      - name: echo-server-container
        image: kennship/http-echo
```

If we apply this, we can then use `kubectl get deploy` to view its status. Note that the config above is identical to that for the ReplicaSet, just with a different `kind`. However, Deployments have a bunch of additional configuration options ([see here][api-deploys]). A couple of note:

- `spec.revisionHistoryLimit`: Deployments can be rolled back (run `kubectl rollout` for more detail), and this allows us to specify how much history to keep around to enable that to take place.
- `spec.strategy`: Configure the update strategy.

# Services

Containers within Pods can communicate to each other via ports on `localhost` (thus, if a Pod contains more than one container, those containers share the same port space). In addition, Pods have an IP address internal to the cluster, so that a container in one Pod can talk to a container in another. We've seen this already using our `busybox` Pod.

The problem is that the Pod's cluster IP address does not persist if the Pod is restarted. If Pods are being managed by Deployments (which is often the case), you'll have several Pods that you want to direct traffic to, and they will be destroyed or created as the Deployment sees fit.

 *Services* make it easy for Pods to communicate with each other and the outside world.

> If a Service is created before the Pods that it would forward traffic to, then when the Pods are created they will be spread across Nodes by default, because Kubernetes understands that the Service will be more resilient if the Pods it forwards traffic to are spread out in this way (however we can use Pod antiAffinity to do explicit about this).

## ClusterIP

The most basic service type is ClusterIP. This Service provides a single persistent IP address internal to the cluster that routes traffic to the Pods selected by it. This IP address will persist until you delete the Service. In addition, DNS records are added so that you can use a name instead of an IP address to talk to the selected Pods.

![A basic ClusterIP Service enabling communicate between Pods][service]

To allow other Pods on the same cluster to communicate with our `echo-server`, we could write a ClusterIP Service like so:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-echo-server-service
spec:
  type: ClusterIP
  selector:
    app: echo-server
  ports:
  - protocol: TCP
    port: 80
    targetPort: 3000
```

If we save that to a file, eg `echo-server-service.yaml`, we can tell Kubernetes about it using `kubectl apply -f echo-server-service.yaml`.

Once applied, `kubectl get services` will list active services and their internal (and external if applicable) IP addresses.

Applying a Service in Kubernetes leads to changes in `iptables` and `resolv.conf` in order that requests that look like they are destined for the Service are actually sent, in a round-robin fashion, to any Pods matching the selector provided in the Service config (in the above example, Pods with an 'app' label set to 'echo-server').

To test this new Service, make sure that the `echo-server` Pod is running again. Then, we can use the same trick as before to give us a Pod running busybox (`kubectl run -it busybox --image=busybox --rm --generator=run-pod/v1`). This time, instead of using the IP address of the Pod itself, we can use the IP address of the Service, and run for example `wget [service-cluster-ip] -O -` to get a result from it. Even better, service names resolve as you might hope via DNS, so we can also use `wget http://my-echo-server-service -O -` to talk to the `echo-server` Pod now. If the Service is in a namespace, it will be accessible via something like `http://service-name.namespace-name`. Since the same service could be running in several namespaces, this allows you to disambiguate which one you want (by default you'll find whatever is running the same namespace if you don't provide one).

Even if we delete and recreate the `echo-server` Pod and it ends up with a different IP address (`kubectl get pods -o wide` to see the Pod IPs), the Service will continue to work. This makes it a durable way for Pods to communicate with each other.

See [here][api-services] for the complete API reference for Services.

## NodePort

The *NodePort* Service type builds upon the ClusterIP Service type and additionally forwards traffic from a given port on the Node itself to the desired Pods.

![A NodePort Service allowing external access to Pods via a port on the Node][nodeport]

We can expose our `echo-server` to the outside world by tweaking our `echo-server-service.yaml` above to look like:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-echo-server-service
spec:
  type: NodePort
  selector:
    app: echo-server
  ports:
  - nodePort: 30001
    protocol: TCP
    port: 80
    targetPort: 3000
```

If we don't provide a `nodePort` ourselves, one will be allocated for us, which we can discover using `kubectl get services`. The `nodePort` must be within the range 30000-32767.

If we apply this, we should find that we can still communicate with our `echo-server` the same way when we spin up our busybox Pod. Additionally, if we know the IP address of one of our cluster Nodes (`minikube ip` provides that for minikube), we can now access our `echo-server` on port 30001 (try browsing to it!) from our local machine.

## LoadBalancer

Services like Google Cloud also offer a *LoadBalancer* Service type, which builds on the NodePort Service and leads to an external IP address being allocated for us. Sending traffic to this address will then load balance it across our different cluster Nodes, which in turn will forward traffic through their exposed ports to the Pods selected by the underlying ClusterIP Service.

A LoadBalancer Service can incur quite a large additional cost, so various tutorials exist about configuring your own load balancing inside your Kubernetes cluster.

The configuration for the LoadBalancer Service type is identical to the NodePorts configuration, just with a different `spec.type` and a couple of additional options (see [the api][api-services]). For our `echo-server` it could look like this:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-echo-server-service
spec:
  type: NodePort
  selector:
    app: echo-server
  ports:
  - nodePort: 30001
    protocol: TCP
    port: 80
    targetPort: 3000
```

Using `minikube`, the 'External IP' field of the LoadBalancer service is stuck in a pending state (it's just the same as `minikube ip`. On other providers, it should show up next to the service in `kubectl get services` once it's been allocated.

# Configuration

We've talked about Pods and Services, which define the things you want to run and how they can communicate with each other. When it comes to configuring these Pods (for example, telling them where to look for things or which port to run on), we can use *ConfigMaps* and *Secrets*.

## ConfigMap

Our various Pods and such will potentially have various configuration that needs setting. Rather than baking this configuration into the containers themselves, we can provide it to Pods using *ConfigMaps*. Our `echo-server` for instance starts on a port defined by the environment variable `PORT`, defaulting to `3000` if it's not provided. We could put this configuration into a ConfigMap like so:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: echo-server-config-v1
data:
  PORT: "3000"
```

We can apply this as usual with `kubectl apply -f the-file-name.yaml`, and we can list our ConfigMaps with `kubectl get configmaps`. Particularly useful for ConfigMaps (but usable everywhere) is `kubectl get configmap echo-server-config-v1 -o yaml`, which prints out the yaml so that we can see the configuration values inside. `kubectl describe configmap echo-server-config-v1` also shows us the data.

With this, we can provide the data in the form of a folder mounted in the container (in which there is a file per variable name which contains the value of that variable), or via environment variables using the `env` or `envFrom` options.

To provide the config to our `echo-server` as environment variables we can edit the `deployment` to look like:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: echo-server
  labels:
    app: echo-server
spec:
  replicas: 3
  selector:
    matchLabels:
      app: echo-server
  template:
    metadata:
      labels:
        app: echo-server
    spec:
      containers:
      - name: echo-server-container
        image: kennship/http-echo
        envFrom:
        - configMapRef:
            name: echo-server-config-v1
```

We could use `env` instead of `envFrom` if we want to map specific config variables to environment variables, rather than provide everything as-is.

If this Deployment is already running, you'll see that applying this updated config will spin down the old Pods and spin up new ones, since the template Pod spec has changed. We can verify that it is still accessible on port 3000 by spinning up a busybox Pod again if we like and using `wget` given the cluster IP address of one of the `echo-server` Pods.

If we want to change the port, a good approach now is to edit the ConfigMap to specify the new port, but also alter its name, so we might end up with:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: echo-server-config-v2
data:
  PORT: "4000"
```

With that applied, we'll have both `echo-server-config-v1` and `echo-server-config-v2` in Kubernetes. If we then edit the `echo-server` Deployment to use `v2` of the config and apply it, it will spin down the old Pods and spin up some new Pods that use the new config. Using `wget` again, we'll now see that port 4000 responds and port 3000 does not (the Service we defined earlier will now not work, since it is still forwarding traffic to port 3000).

Because we have kept the old config around, and explicitly changed the deployment pod spec to use the new config, we can also rollback the deployment very easily if we realise we've made a mistake with the new config.

`kubectl rollout history deploy echo-server` will list the revisions. adding eg `--revision=2` lets you inspect the details. We can undo a rollout of a Deployment using, in this case, `kubectl rollout undo deploy echo-server`, optionally adding `--to-revision=N` to roll back to a specific revision `N`. eventually, when we are happy with the new config, we can manually `kubectl delete configmap echo-server-config-v1` if we like (noting that we'll be unable to rollback to anything using it once it is gone).

## Secret

*Secrets* are fundamentally very similar to ConfigMaps, but make it slightly harder to expose the secret information. One difference is that data is always base64 encoded, presumably so that it's slightly harder to read it from the API. If you need to provide sensitive information to containers, use these instead of ConfigMaps.

# Conclusion

The basic building block of Kubernetes is the Pod, which defines the Docker images to run inside it. We can manage the life cycle of Pods using Deployments, and we can describe how Pods can talk to each other using Services. Finally, we can pass in configuration to Pods to avoid needing to base it into the images that they run.

There's loads of stuff I haven't gone into any detail at all about however, including:

- `PersistentVolumes` and `PersistentVolumeClaims`, which allow you to define persistent storage that is available to the cluster, and make use of it (claim it) by specific Pods. Using this, we can run things like databases and know that the underlying data will survive Pod restarts and such.
- `StatefulSets`, which are like Deployments but are more regular in how they are spun up and down, their network addresses, and what storage they are given, so that you can run stateful applications a little easier (like databases). I had a cursory play with these but that's all.
- `DaemonSets`, which run one instance of a Pod per node.
- `Jobs`, which manage running Pods that expect to complete.
- Automatic scaling, both in terms of the number of Pods that a Deployment is running and the number of nodes running in your cluster.

And much more.

That said, I hope that I've provided enough of a foundation that you can go away and learn about the things you've missing out on.

Good luck Kubernetesing!

[kube-cluster]: cluster.svg
[service]: services.svg
[nodeport]: nodeport.svg

[api-pods]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.13/#pod-v1-core
[api-services]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.13/#service-v1-core
[api-deploys]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.13/#deploymentspec-v1-apps
[minikube]: https://kubernetes.io/docs/setup/minikube/
[kubectl]: https://kubernetes.io/docs/tasks/tools/install-kubectl/
[json2yaml]: https://www.json2yaml.com/
[dockerhub]: https://hub.docker.com/search?q=&type=image