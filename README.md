# Akka Copycat

Based on JavaCopycat, it is a translation in Scala using Akka. For the moment the actors are:

- WebSocketActor: to communicate with UI
- ExecutionRun
- Coderack
- Workspace
- Slipnet
- Temperature
- All Codelets: BondBuilder, BottomeUpcorrespondenceScout, ....

Codelet execution goes like this:

1. Bunch of message to Workspace or Slipnet with for each a response to codelet who decide for the follow up.
2. Codelet receive a Finished message
3. Codelet a ChooseAndRun to the Coderack 

## References

| Name                             | URL                                                          |
| -------------------------------- | ------------------------------------------------------------ |
| SVG library comparison           | https://www.webdesignerdepot.com/2018/02/8-best-free-libraries-for-svg/ |
|                                  | https://noeticforce.com/javascript-libraries-for-svg-animation |
| Melanie Mitchell talk            | https://www.youtube.com/watch?v=ImKkaeUx1MU                  |
| CopyCat gentle overview          | [file:///Users/dschoen/Documents/gitlab/copycat/JavaCopyCat/doc/Tutorial/overvw.htm](file:///Users/dschoen/Documents/gitlab/copycat/JavaCopyCat/doc/Tutorial/overvw.htm) |
| Monitoring                       | https://kamon.io/apm/pricing/<br />https://sematext.com/pricing/#logsene |
| Synch tasks from mutliple actors | https://stackoverflow.com/questions/22770927/waiting-for-multiple-results-in-akka |
|                                  | https://www.scientificamerican.com/article/experts-short-term-memory-to-long-term/ |

## SVG Library for Scala.js

| Name             | Facade of... | Description                                        |                                                  |
| ---------------- | ------------ | -------------------------------------------------- | ------------------------------------------------ |
| Paths.scala.js   | Paths.js     | generate SVG paths                                 | https://github.com/andreaferretti/paths-scala-js |
| scala-js-snapsvg | Snap.svg     |                                                    | https://github.com/akauppi/scalajs-snapsvg       |
| Checkers         |              | Good example of play app with diode, scalajs react | https://github.com/kschuetz/checkers             |
|                  |              |                                                    | https://di-in-scala.github.io/                   |

## Notes

### JavaCopyCat Overview

Analogies relies on 2 mental processes:

- **Representation formation**: Before mapping, objects has to be perceived. Role of object is specified. Example: identify 2 letters as being the rightmost letter in the string.
- **mapping**: objects are equated because they are perceived as "playing the same role"

#### Architecture of Copycat

- The Workspace: "working memory" where "thoughts come to mind"
- The Slipnet: "long-term memory" (but during a run) concepts.If the concept is more active, more workers are assigned to the task.
- Coderack: Information on all the workers (=Codelet) in the WorkSpace. A codelet execute a piece of code that corresponds with the job that they have been assigned.
- Temperature: how globally consistent is the structure build. Used to dertermine when the program should stop 

# Concept Mapping

where are they created ?

| Where (class)   |                          | Remark          |
| --------------- | ------------------------ | --------------- |
| Correspondence  | get_incompatible_bond    | created locally |
| Concept_mapping | get_concept_mapping_list |                 |
|                 |                          |                 |

# Slipnet

Workspace modify the slipnet. 

We can query the slipnet for influencing the workspace.

# Execution Debugging

## Run JavaCopycat

In Intellij, go to src/rungroup and right-click "main(String[]): void" and choose "Run 'rungroup.main()'"

It execute 1000 trials using different random seed

Output will look like this:

```
abc
abd
ijk
-------------------------------------------- 0
NUM NUM 0
ijl 266 12.098747680049227
-------------------------------------------- 1
NUM NUM 1
ijd 221 12.104129516579881
-------------------------------------------- 2
NUM NUM 2
ijl 167 10.937297283098673
...
...
-------------------------------------------- 999
NUM NUM 999
ijl 288 12.834735122641115

Process finished with exit code 0
```

It will give exactly always the same, see JavaCopyCat1000Trial.log

Here the stats:

| Pattern | Number |
| ------- | ------ |
| ijl     | 504    |
| ijd     | 485    |
| ijk     | 11     |



## Codelet executed

| Execution 1                                                  | Execution 2                              |
| ------------------------------------------------------------ | ---------------------------------------- |
| ReplacementFinder (e, initial)                               | BottomUpCorrespondenceScout (g, initial) |
| BottomUpCorrespondenceScout (i, initial)                     | BottomUpCorrespondenceScout (i, created) |
| BottomUpBondScout (a, initial)                               | BottomUpBondScout (a, initial)           |
| BottomUpCorrespondenceScout (created)                        | ReplacementFinder(d, initial)            |
| BottomUpBondScout (b, initial)                               | ReplacementFinder (f, created)           |
| BottomUpBondScout (c, initial)                               | ReplacementFinder(e, initial)            |
| ReplacementFinder (f, initial)                               | BottomUpBondScout(c, initial)            |
| BottomUpCorrespondenceScout (g, initial)                     | BottomUpBondScout(b, initial)            |
| ReplacementFinder (d, initial)                               | BottomUpCorrespondenceScout(h, initial)  |
| ChooseAndRun: codelets is empty => stop<br />Why is it stopping ? Probably because not all codelets are implemented |                                          |

