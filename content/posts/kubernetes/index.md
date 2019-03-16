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

```
apiVersion: v1
kind: Pod
metadata:
  name: echo-server
  labels:
    app: echo-server
spec:
  Containers:
  - name: echo-server-Container
    image: kennship/http-echo
```

Here, we give the Pod a name of "echo-server" and give it a single key-value label of `app: echo-server`. We could add labels specifying the version of our app and so forth as well if we like. We can use labels to select matching Pods in various scenarios, so they are really useful. Finally, we provide a spec describing the Containers we want to run. The image name refers to an image on Docker Hub by the same name.

If we save this to a file, eg `echo-server.yaml`, we can run it in our Kubernetes cluster by running `kubectl apply -f echo-server.yaml`. This command sends whatever configuration file (or folder) you point it at to Kubernetes, which takes care of making reality reflect it.

`kubectl get pods` should now list a single Pod, `echo-server` as running. `kubectl get pods -o wide` shows some extra information, including the Pods' cluster IP addresses.

To test this echo server, we can spin up an interactive `busybox` prompt in another Pod with the following:

```
kubectl run -i -t busybox --image=busybox --rm
```

This should give us a prompt inside a Pod running `busybox`. Now, if we take the IP address from the earlier command of the echo pod, we can ping the echo Pod and see what happens by typing something like `wget 172.17.0.5:3000 -O -` (replacing that IP address with the actual one from above).

Some other interesting commands you can run:

`kubectl describe pod echo-server` describes the `echo-server` Pod, including showing a recent event log. This is a handy command if you want to debug why a Pod is not running properly.

`kubectl get pod echo-server -o yaml` outputs the current yaml configuration stored for the `echo-server` Pod. You'll see loads more than our tiny config, including various defaults and current Pod status information, which is all written to the configuration in Kubernetes as the Pod runs.

Finally, to remove the echo server Pod from our cluster, we can run `kubectl delete pod echo-server`.

# Services

Containers within Pods can communicate to each other via ports on `localhost` (thus, if a Pod contains more than one Container, those Containers must be careful not to overlap in the ports they bind to).

Pods also have an IP address internal to the cluster when they are spun up. If a Container within a Pod binds to a port, other Pods on the same cluster will be able to talk to it via this.

The problem is that the Pod's cluster IP address does not persist if the Pod is restarted and so on. Services, on the other hand, have a persistent IP address (and cluster-internal dns name). They therefore make it easy for Pods within a cluster to communicate with eachother, as well as exposing certain ports on Pods to the outside world and more.

A very basic Service for the `echo-server` configuration above might look like this:

```
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

Applying a Service in K8s leads to changes in `iptables` and `resolv.conf` rules in order that requests that look like they are destined for the Service are actually sent, in a round-robin fashion, to any Pods matching the selector provided in the Service config.

To test this new Service, make sure that the `echo-server` Pod is running again. Then, we can use the same trick as before to give us a Pod running busybox (`kubectl run -i -t busybox --image=busybox --rm`). This time, instead of using the IP address of the Pod itself, we can use the IP address of the Service, and run for example `wget SERVICE-CLUSTER-IP -O -` to get a result from it. Even better, service names resolve as you might expect via DNS, so we can also use `wget http://my-echo-server-service -O -` to talk to the `echo-server` Pod now. Even if we delete and re-apply the `echo-server` config and it ends up with a different IP address (`kubectl get pods -o wide` to see the Pod IPs), the Service will continue to work.