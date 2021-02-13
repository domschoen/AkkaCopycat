package models

object SlipnetFormulas {

    def get_bond_category(fromnode: SlipNode, tonode: SlipNode, identity: SlipNode): Option[SlipNode] = {
      // return the label of the link between these nodes if it exists
      if (fromnode==tonode) {
        Some(identity)
      } else {
        fromnode.outgoing_links.find(l => l.to_node == tonode).map(_.label)
       }
    }

    def slip_linked(s1: SlipNode, s2: SlipNode): Boolean = {
      s1.lateral_slip_links.find(sl => sl.to_node==s2).isDefined
    }

    def get_related_node(category: SlipNode, relation: SlipNode, identity: SlipNode): Option[SlipNode] = {
      // return the node that is linked to this node via this relation
//      System.out.println("get_related_node " + relation + " identity " + identity);

      if (relation==identity) {
        Some(category)
      } else {

        category.outgoing_links.find(l => {
//          System.out.println("get_related_node category.outgoing_links l.label " + l.label);

          l.label == relation
        }).map(_.to_node)
      }
    }

    def linked(s1: SlipNode, s2: SlipNode): Boolean = {
      s1.outgoing_links.find(s => s.to_node==s2).isDefined
    }

    def related(s1: SlipNode, s2: SlipNode): Boolean = {
      (s1==s2) || linked(s1,s2)
    }

}
