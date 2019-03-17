+++
title = "Notes on Kubernetes"
description = "Some notes I wrote up on Kubernetes from having attended the beginner and intermediate training courses hosted by JetStack at the Google building in London."
date = 2019-03-16
[extra]
created = "2019-03-16"
+++

Kubernetes is a Container orchestration platform. In plain english, you use Kubernetes by describing what you want to run in the form of configuration files, and Kubernetes takes care of making sure that what *is* running lines up with the configuration you've given it.

# Prerequisites

If you'd like to follow along with the examples, you should install `kubectl` and set up a Kubernetes cluster somewhere. The easiest way to do this is to install and run `minikube`, which starts up a small single-node cluster on your machine that you can play with. Alternately you can spin up a cluster on Google Cloud or some other provider, and everything should work in the same way.

These notes and examples are based on `kubectl` version 1.13 and `minikube` version 0.35.

# Overview

Life starts with a *Kubernetes Cluster*. This consists of a set of *nodes* (physical machines, for instance) that actually run your things, and one or more *masters* that coordinate the running of your things.

Each master has a copy of a distributed key-value store called *etcd*, which is where the configuration describing what should be running lives. You control Kubernetes by editing this configuration to describe what you want reality to look like. Things watching that configuration on the master then go forth to make reality match it.

The configuration files you write to describe your kubernetes environment are written in YAML, which is a superset of JSON.

At a basic level, a running Kubernetes cluster consists of various *Pods*, which are each running one or more applications. These Pods can communicate with each other and with the outside world via *Services*. We can then use *Controllers*, which manage starting and stopping groups of Pods and make it easy to perform rolling updates and ensure a certain number of Pods are always running. Finally, we can describe available PersistentVolumes to our cluster and attach them to Pods via PersistentVolumeClaims in order to have persistent storage for things.

I'll go into a bit more detail about these things below.

# Pods

A *Pod* is the basic building block in kubernetes that describes something you want to run (for example, a database or a web server or anything else).

A Pod can contain one or more *Containers* (often just one). A Container is the running instance of an *image* (usually a Docker image). Basically, this means that if you want to get something running in Kubernetes, you'll typically take the following steps:

* Write a `Dockerfile` for your app describing how to package it up into a docker image that contains everything necessary to run it.
* Push the docker image to some place on the internet that your kubernetes cluster can access (this is reminiscent of pushing code to github for instance; you can also `pull` images from the internet to run them in locally).
* When describing a Pod you'd like to run in Kubernetes, you can then point to the URL that your image will be available from. Kubernetes will then be able to acquire the image and spin it up whenever it needs to.

Here is the configuration for a Pod that simply runs an echo server on port `3000` (by default):

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: echo-server
  labels:
    app: echo-server
spec:
  Containers:
  - name: echo-server-container
    image: kennship/http-echo
```

Here, we give the Pod a name of "echo-server" and give it a single key-value label of `app: echo-server`. We could add labels specifying the version of our app and so forth as well if we like. We can use labels to select matching Pods in various scenarios, so they are really useful. Finally, we provide a spec describing the Containers we want to run. The image name refers to an image on Docker Hub by the same name.

If we save this to a file, eg `echo-server.yaml`, we can run it in our Kubernetes cluster by running `kubectl apply -f echo-server.yaml`. This command sends whatever configuration file (or folder) you point it at to Kubernetes, which takes care of making reality reflect it.

`kubectl get pods` should now list a single Pod, `echo-server` as running. `kubectl get pods -o wide` shows some extra information, including the Pods' cluster IP addresses.

To test this echo server, we can spin up an interactive `busybox` prompt in another Pod with the following:

```
kubectl run -it busybox --image=busybox --rm
```

This should give us a prompt inside a Pod running `busybox`. Now, if we take the IP address from the earlier command of the echo pod (this IP address is internal to the cluster), we can ping the echo Pod from our `busybox` one and see what happens by typing something like `wget POD_IP_ADDRESS:3000 -O -`.

Some other useful commands:

`kubectl describe pod echo-server` describes the `echo-server` Pod, including showing a recent event log. This is a handy command if you want to debug why a Pod is not running properly.

`kubectl get pod echo-server -o yaml` outputs the current yaml configuration stored for the `echo-server` Pod. You'll see loads more than our tiny config, including various defaults and current Pod status information, which is all written to the configuration in Kubernetes as the Pod runs.

`kubectl exec -it echo-server bash` runs a command inside the `echo-server` Pod's only container (you can specify the container to run the command in as well if need be). In this case, we specify `-it` to get an interactive terminal, and run `bash` so that we can snoop around.

`kubectl attach -it busybox-55c7c8dc95-rdnbb` (where we need `kubectl get pods` to find the actual busybox Pod name) will attach to the running process in the busybox Pod, assuming that it is currently running at all.

`kubectl delete pod echo-server` deletes the `echo-server` Pod.

There's loads more that you can configure about Pods and the Containers that live inside them; you can peruse the full API [here][api-pods]. Here are some interesting ones for Pods:

- `spec.volumes`: Make volumes available to the Pod that can then be mounted into the Containers inside it.
- `spec.affinity`: Decide which other Pods or Nodes this Pod will be scheduled near to or away from. Sub properties include:
  - `podAffinity`/`podAntiAffinity`: Describe which other Pods you want the Pod to be spun up next to or to avoid. You use labels to select other Pods, and a `topologyKey` to select which label on the Node you're trying to match up in order to know where to schedule the Pod. Using this, you can make sure Pods are spun up on the same machine, or just the same geographic Region, or anything else based on the labels you give your Nodes.
  - `nodeAffinity`: Schedule the Pod onto Nodes matching the labels selected.
- `spec.tolerations`: Nodes can be tainted to avoid things being scheduled onto them (for example, maybe you have a high spec Node that you only want specific jobs to run on). `tolerations` allow a Pod to be scheduled onto one of these tainted Nodes.
- `spec.initContainer`: If we define this container, the Pod won't be marked as ready until this container has successfully completed.

And for containers:

- `resources`: Define the resources (CPU/memory) that you expect the Container, and limits before the Container will be throttled/killed. The resources are defined in terms of the CPU/memory available on the Node the Pod is running on.
  - `requests`: Define what you expect the Pod to consume ordinarily. The Kube scheduler uses this information to decide on which Node it can fit the Pod.
  - `limits`: Define at which point to throttle the Container or kill it. This is useful to kill Containers that get stuck into infinite loops or leak memory for instance.
- `volumeMounts`: For the `volumes` that we make available in the Pod spec, we can mount them into specific places in the Container.
- `env`: Set environment variables for the Container.
- `envFrom`: Inject environment variables from a `configMap` or `secretMap` (these are things you can write YAML config to create, and contain lists of useful variables and such that you can share across Containers).
- `command`/`args`: Run a command when the container starts up, or provide args to override the command that was already configured to run from the Docker image.
- `readinessProbe`/`livenessProbe`: Define when the Container is ready to start receiving traffic and whether it's still alive. These can be a command that needs to run on the container, or an HTTP GET request to perform against it, for instance. A Pod is only marked as ready once all of the containers in it are. The Container is restarted (not the Pod) if `livenessProbe` fails.

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

Controllers in Kubernetes ensure that the current state of the system matches the configured state. So far, we've only looked at Pods. One of the issues with creating Pods manually is that if they are terminated for whatever reason, they won't be recreated. Thus, it's usually better to create *Deployments*, which themselves coordinate the creation of *ReplicaSets*, whose job it is to make sure a certain number of copies of some Pod are always running.

While you can create ReplicaSets directly, Deployments are more powerful, and almost always what you want to be using.

## ReplicaSet

The job of a ReplicaSet is to keep track of how many of some Pod are running (given some `selector` which matches against Pod labels). if that number goes above the number of `replicas` provided, it will delete Pods. If the number of Pods running dips below the desired `replicas` count, the ReplicaSet will create new Pods based on the Pod `template` provided.

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

Once we have saved and applied this, `kubectl get replicasets` will now show this new ReplicaSet, along with the state of the underlying Pods. If we run `kubectl get pods`, we'll see several Pods of the above spec, each with a random hash appended to them so that they have unique identifiers. If we `kubectl delete` one of these Pods, we'll see that a new one is spun up in its place. You'll need to delete the ReplicaSet in order to permanently delete the associated Pods.

If we edit the number of `spec.replicas` in our config and `kubectl apply` it again, the state of our system will update to match that new replica count.

`kubectl edit replicaset echo-server` is another way to edit the details of our ReplicaSet (or indeed any other object in Kubernetes), but it generally now recommended as your local config files will then be out of sync with the actual state of the system.

If we edit the Pod template spec this won't change any of the existing Pods. Instead, new Pods that are spun up will be based on the new template. This is not recommended though; it can lead to an inconsistent state whereby you have several slightly different Pods under a given ReplicaSet. This is where *Deployments* come in.

## Deployment

The job of a Deployment is to manage ReplicaSets. Each time we want to change the Pod spec (for instance, we want to update the container our Pods are running), the Deployment will spin down the existing ReplicaSet and spin up a new one according to the update strategy we have provided (by default, it'll perform a rolling update, gradually replacing old for new Pods).

When you create a Deployment, you'll find that a corresponding ReplicaSet has been created (with a random hash appended to it as it's managed by the Deployment), and the corresponding Pods are created (with two random hashes appended for the ReplicaSet and Deployment they relate to).

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

If we apply this, we can then use `kubectl get deploy` to view its status. Note that the config above is identical to that for the ReplicaSet, just with a different Kind. However, Deployments have a bunch of additional configuration options (see [here][api-deploys]).

- `spec.revisionHistoryLimit`: Deployments can be rolled back (run `kubectl rollout` for some commands you can run), and this allows us to specify how much history to keep around for that.
- `spec.strategy`: Configure the update strategy.

# Services

Containers within Pods can communicate to each other via ports on `localhost` (thus, if a Pod contains more than one Container, those Containers share the same port space). Pods have an IP address internal to the cluster, so that a Container in one Pod can talk to a container in another via this.

The problem is that the Pod's cluster IP address does not persist if the Pod is restarted. If Pods are being managed by Deployments (which is often the case), you'll have several Pods that you want to direct traffic to, and they could come and go at a moments notice.

 *Services*, on the other hand, have a persistent IP address (and cluster-internal dns name). They therefore make it easy for Pods within a cluster to communicate with each other, as well as exposing certain ports on Pods to the outside world and more.

If a Service is created before the Pods that it matches on, then when the Pods are created they will be spread across Nodes by default, because Kubernetes understands that the Service will be more resilient if the Pods it forwards traffic to are spread out in this way.

## ClusterIP

The most basic service type is ClusterIP. This Service provides a single IP address internal to the cluster that routes traffic to the Pods selected by it.

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

To test this new Service, make sure that the `echo-server` Pod is running again. Then, we can use the same trick as before to give us a Pod running busybox (`kubectl run -i -t busybox --image=busybox --rm`). This time, instead of using the IP address of the Pod itself, we can use the IP address of the Service, and run for example `wget SERVICE-CLUSTER-IP -O -` to get a result from it. Even better, service names resolve as you might expect via DNS, so we can also use `wget http://my-echo-server-service -O -` to talk to the `echo-server` Pod now. If the Service is in a namespace, it will be accessible via something like `http://service-name.namespace-name`. Since the same service could be running in several namespaces, this allows you to disambiguate which one you want.

Even if we delete and re-apply the `echo-server` config and it ends up with a different IP address (`kubectl get pods -o wide` to see the Pod IPs), the Service will continue to work.

See [here][api-services] for the complete API reference for Services.

## NodePort

The *NodePort* Service type builds upon the ClusterIP Service type and additionally forwards traffic from a given port on the Node itself to the desired Pods. We can expose our `echo-server` to the outside world by tweaking our `echo-server-service.yaml` above to look like:

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

If we apply this, we should find that we can still communicate with it the same way when we spin up our busybox Pod. Additionally, if we know the IP address of one of our cluster Nodes (`minikube ip` provides that for minikube), we can now access our `echo-server` on port 30001 (try browsing to it!) from our local machine.

## LoadBalancer

Services like Google Cloud also offer a *LoadBalancer* Service type, which builds on the NodePort Service and leads to an external IP address being allocated for us. Sending traffic to this address will then load balance it across our different cluster Nodes, which in turn will forward traffic through their exposed ports to the Pods selected by the underlying ClusterIP Service.

A LoadBalancer Service can incur quite a large additional cost, so various tutorials exist about configuring your own load balancing inside the Kubernetes cluster.

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

Our various Pods and such will potentially have various configuration that needs setting. Rather than baking this configuration into the Containers themselves, we can provide it to Pods using ConfigMaps and SecretMaps. Our `echo-server` for instance starts on a port defined by the environment variable `PORT`, defaulting to `3000` if it's not provided. We could put this configuration into a ConfigMap like so:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: echo-server-config-v1
data:
  PORT: "3000"
```

We can apply this as usual with `kubectl apply -f the-file-name.yaml`, and we can list our ConfigMaps with `kubectl get configmaps`. Particularly useful for ConfigMaps (but usable everywhere) is `kubectl get configmap echo-server-config-v1 -o yaml`, which prints out the yaml so that you can see the configuration values inside. `kubectl describe configmap echo-server-config-v1` also shows you the data.

With this, we can provide the data as a mount point (in which there is a file per variable name which contains just the value), or via environment variables using the `env` or `envFrom` options.

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

If this is already running, you'll see that applying this updated config will spin down the old Pods and spin up new ones, since the template Pod spec has changed. We can verify that it is still accessible on port 3000 by spinning up a busybox Pod again if we like (`kubectl run -it busybox --image=busybox --rm`) and using `wget` on one of the `echo-server` Pods.

If we want to change the port, a good approach now is to edit the ConfigMap to specify the new port, but also alter its name, so we might end up with:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: echo-server-config-v2
data:
  PORT: "4000"
```

With that applied, we would end up with a `v1` and a `v2` of the ConfigMap in Kubernetes. If we then edit the deployment to use `v2` of the config and apply it, it will spin down the old Pods and spin up some new Pods that use the new config. Running `wget` directly to one of the Pods, we'll now see that port 4000 responds and port 3000 does not (the Service we defined earlier will now not work, since it forwards traffic to port 3000).

Because we have kept the old config around, and explicitly changed the deployment pod spec to use the new config, we can also rollback the deployment very easily if we realise we've made a mistake with the new config.

`kubectl rollout history deploy echo-server` will list the revisions. adding eg `--revision=2` lets you inspect the details. We can undo a rollout of a Deployment using, in this case, `kubectl rollout undo deploy echo-server`, optionally adding `--to-revision=N` to roll back to a specific revision N. eventually, when we are happy with the new config, we can manually `kubectl delete configmap` the old versions if we like (noting that we'll be unable to rollback to anything using them once they are gone).

## Secret

*Secrets* are fundamentally very similar to ConfigMaps, but make it slightly harder to expose the secret information. One difference is that data is always base64 encoded, presumably so that it's slightly harder to read it from the API.

# Conclusion

The basic building block of Kubernetes is the Pod, which defines the (docker) images to run as containers inside it. We can manage the lifecycle of Pods using Deployments, and we can describe how Pods can talk to each other using Services. Finally, we can pass in configuration to Pods to avoid needing to base it into the images that they run.

There's loads of stuff I haven't gone into any detail at all about however, including:

- `PersistentVolumes` and `PersistentVolumeClaims`, which allow you to define persistent storage that is available to the cluster, and claim it by specific Pods. Using this, we can run things like databases and know that they will survive Pod restarts and such. I didn't play with this stuff as much though during the training course!
- `StatefulSets`, which are like Deployments but are more regular in how they are spun up and down, and what storage they are given, so that you can run stateful applications a little easier (like databases). Once again, I had a cursory play with these but not much.
- `DaemonSets`, which run one instance of a Pod per node
- `Jobs`, which manage running Pods that expect to complete.

And much more.

That said, I hope that I've provided enough of a foundation that you can go away and learn about the things you've missing out on.

Good luck Kubernetesing!

[api-pods]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.13/#pod-v1-core
[api-services]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.13/#service-v1-core
[api-deploys]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.13/#deploymentspec-v1-apps