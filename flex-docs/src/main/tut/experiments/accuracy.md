---
layout: docs
title:  "Accuracy on Data Streams"
section: "experiments"
mathjax: true
---

{% include mathjax.html %}

# Accuracy on Data Streams

We compare the accuracy of `Sketch` with that of the alternatives by measuring statistical distance for various types of stationary and non-stationary data streams. Moreover, we compare the characteristics for a non-stationary case of `Sketch` with that of the alternatives. The results show that `Sketch` has its advantages and disadvantages in estimating the distribution of various types of stationary data streams. `Sketch` and SPDTw are more useful for a non-stationary data stream as they have superior accuracy compared with oKDE, except for the case of sudden concept drift.

Note that the parameters of SPDTw are deliberately selected so as to have similar accuracy as `Sketch` when concept drift occur. We observe that SPDTw is 2.0--3.2$\times$ inaccurate than `Sketch` when it estimates PDF for a stationary data stream. Moreover, this results in a significant increase in the throughput and memory usage of SPDTw.






