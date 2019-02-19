# Docs

## Build the microsite

Once you have written your documents, you can build the microsite running this sbt task:

```bash
sbt> makeMicrosite
```

Internally, it'll sequentially run other tasks including its own, [`tut`](https://github.com/tpolecat/tut), and `makeSite` ([sbt-site](https://github.com/sbt/sbt-site)) tasks.

## View the microsite in your browser

If you're running the microsite locally, you can follow these steps:

1. In a shell, navigate to the generated site directory in `target/site`.

2. Start Jekyll with `jekyll serve`.

3. Navigate to [http://localhost:4000/yourbase_url/](http://localhost:4000/yourbase_url/) in your browser, where `yourbase_url` depends on your own preferences (see `micrositeBaseUrl` setting). Note, if you haven't specified any `micrositeBaseUrl` setting, it'll be empty by default so you can navigate to the site following this url [http://localhost:4000](http://localhost:4000/).  


## Publish the microsite

You can just run (with authorization):

```bash
sbt> publishMicrosite
```

And that's all. Behind the scenes, `makeMicrosite` and `pushSite` are invoked. 

For more detail, see [sbt-microsite docs](https://47deg.github.io/sbt-microsites/docs/).