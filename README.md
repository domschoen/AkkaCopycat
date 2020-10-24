# Akka Copycat

This tool will configure the JavaMonitor in a state which ensure that a defined number of instances are running on each host for an application.

We can do a bounce followed by a ensure as described above.

## References

| Name                             | URL                                                          |
| -------------------------------- | ------------------------------------------------------------ |
| SVG library comparison           | https://www.webdesignerdepot.com/2018/02/8-best-free-libraries-for-svg/ |
|                                  | https://noeticforce.com/javascript-libraries-for-svg-animation |
| Melanie Mitchell talk            | https://www.youtube.com/watch?v=ImKkaeUx1MU                  |
| CopyCat gentle overview          | [file:///Users/dschoen/Documents/gitlab/copycat/JavaCopyCat/doc/Tutorial/overvw.htm](file:///Users/dschoen/Documents/gitlab/copycat/JavaCopyCat/doc/Tutorial/overvw.htm) |
| Monitoring                       | https://kamon.io/apm/pricing/<br />https://sematext.com/pricing/#logsene |
| Synch tasks from mutliple actors | https://stackoverflow.com/questions/22770927/waiting-for-multiple-results-in-akka |
|                                  |                                                              |

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

