# spek

[![Slack](http://spekinator.heroku.com/badge.svg)](http://spekinator.heroku.com)

C# derived programming language inspired from Microsoft's experimental Axum language.

## overview

At this point in time, this language should only be considered a toy language, used in academic or experimental environments only.

I'm currently in the research phase of the project, trying to determine what all I'll need to implement the language. Obviously the grammar is first, but then there is the method by which Axum is compiled to MSIL. At the current moment, I hope to simply make Axum compile into C#, which will then get compiled to MSIL. Hopefully once the dust settles on the Roslyn project, maybe I can adopt the work from there to make Spek work more like a first class citizen to Visual Studio. 

After you view some of the example(s) below on this page, you should feel that it doesn't seem like it's that far away from C#, which is by design. There are some concepts like domain, agent, and channel that may appear to be comparable to concepts in C# like namespace->domain, class->agent, and interface->channel, but it's not that cut and dry.

I'd like to go into these concepts at some point in more detail for my implementation of it, but it's all pretty much layed out in the [Axum Language Overview](http://download.microsoft.com/download/B/D/5/BD51FFB2-C777-43B0-AC24-BDE3C88E231F/Axum%20Language%20Spec.pdf "Axum Language Overview") by Niklas Gustafsson.

If you are interested in helping rebuild the language, feel free to dig in.

### example

Starting with the ever popular example

    public domain MainDomain
    {
        public agent MyMainAgent : channel Application
        {
            public MyMainAgent()
            {
                var cmdArgs = receive(primary::Start);
                Console.WriteLine("Hello World!");
                primary::Done <-- 0;
            }
        }
    }
    
## direction

The intended direction for the project is to follow the same syntax and idioms as Microsoft's implementation of Axum, with some small changes in how schemas and channels are defined and utilized. If you'd like to know more about Axum, feel free to Google it or check out the [Wikipedia](http://en.wikipedia.org/wiki/Axum_(programming_language)) page for it.

One of the issues I've always had with the Axum implementation is the way channels were accessed. It seemed like it was the roughest part of the language's syntax, so at least in this implementation I'd like to make the channel accessible as though it was part of the agent like the Session variable on a MVC controller (or something similar). Possibly through the use of a lambda statement where the channel operators specified in Axum are maintained. I think this would allow for some fairly expressive statements in Linq like statements.

    public domain MainDomain
    {
        public agent MyMainAgent : channel Application
        {
            public MyMainAgent()
            {
                var cmdArgs = this.Receive(x --> x.Start);
                Console.WriteLine("Hello World!");
                this.Send(x <-- x.Done(0));
            }
        }
    }
    
    
## getting started

The language grammar is being developed first, with the assistance of ANTLR and the Visual Studio plugin that is available. The directions below show how to get ANTLR configured with Visual Studio.

### step 1: install java

The C# target for ANTLR 4 requires Java for *compiling* applications. The resulting compiled C# applications will not require Java to be installed (at least not due to the use of ANTLR 4). You can install *any* of the following versions of Java to use this target.

If you already have one of the following installed, you should check to make sure the installation is up-to-date.

* Java 7 runtime environment (x86 or x64)
* Java 7 development kit (x86 or x64, provided that the JRE option is also installed during the development kit installation)
* Java 6 runtime environment (x86 or x64)
* Java 6 development kit (x86 or x64, provided that the JRE option is also installed during the development kit installation)

### step 2: install ANTLR Language Support for Visual Studio (optional)

This step is optional, but highly recommended for users working with a version of Visual Studio that the extension supports. If you have one of the Express Editions of Visual Studio, skip this step.

1. Open Visual Studio
2. Select **Tools** &rarr; **Extensions and Updates...**
3. In the left pane, select **Online**
4. In the top right, type **ANTLR** to search for extensions
5. If your version of Visual Studio is supported, the results pane will show the extension **ANTLR Language Support** by **Sam Harwell**. You can click the result and then select **Download** to install the extension.
6. Restart Visual Studio after installing the extension

### step 3: fork the project

And then just open the .g4 file.
