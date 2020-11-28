      if (name.equals("top-down-bond-scout--direction")){
         slipnode direction = (slipnode) arguments.elementAt(0); 

         print("trying to build a "+direction.pname+" bond");
         double i_relevance = workspace_formulas.local_direction_category_relevance(
                                 workspace.initial,direction);
         double t_relevance = workspace_formulas.local_direction_category_relevance(
                                 workspace.target,direction);
         double i_unhappiness = workspace.initial.intra_string_unhappiness;
         double t_unhappiness = workspace.target.intra_string_unhappiness;
         // choose string

         //print("about to choose string:");
         //print("initial string: relevance="+i_relevance+", unhappiness="+i_unhappiness);
         //print("target string: relevance="+t_relevance+", unhappiness="+t_unhappiness);

         workspace_string string = workspace.initial;
         if ((random.rnd()*(i_relevance+i_unhappiness+t_relevance+t_unhappiness))>
             (i_relevance+i_unhappiness)) string = workspace.target;
         if (string==workspace.initial) print("initial string selected");
         else  print("target string selected");                 

         workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",string.objects);
         print("initial object: "+fromob);
         // choose neighbour
         workspace_object toob = null;
         if (direction==slipnet.left) toob=workspace_formulas.choose_left_neighbor(fromob);
         else toob=workspace_formulas.choose_right_neighbor(fromob);
         if (toob==null) {print (fromob+" has no neighbour: Fizzle!");
                          return false; }
         print("to object: "+toob);
         slipnode bond_facet = workspace_formulas.choose_bond_facet(fromob,toob);
         if (bond_facet==null) { print ("no possible bond-facet: Fizzle!");
                                 return false; }
         print("chosen bond facet = "+bond_facet.pname);
         slipnode from_descriptor = workspace_formulas.get_descriptor(fromob,bond_facet);
         slipnode to_descriptor = workspace_formulas.get_descriptor(toob,bond_facet);
         if ((from_descriptor==null)||(to_descriptor==null)){
            print("both objects do not have this descriptor: Fizzle!");
            return false;
         }
         print("from descriptor: "+from_descriptor.pname);
         print("to descriptor: "+to_descriptor.pname);
         slipnode bond_category = slipnet_formulas.get_bond_category(from_descriptor,to_descriptor);
         if (bond_category==slipnet.identity) bond_category = slipnet.sameness;
         if ((bond_category==null)){
            print("no suitable link: Fizzle!");
            return false;
         }
         // there is a possible bond, so propose it
            print(bond_category.pname+" bond proposed");
         coderack.propose_bond(fromob,toob,bond_category,bond_facet,from_descriptor, to_descriptor,this);
      }
