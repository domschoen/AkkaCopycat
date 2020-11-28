      if (name.equals("top-down-bond-scout--category")){
         slipnode bond_category = (slipnode) arguments.elementAt(0); 
         print("searching for "+bond_category.pname);                 

         double i_relevance = workspace_formulas.local_bond_category_relevance(
                                 workspace.initial,bond_category);
         double t_relevance = workspace_formulas.local_bond_category_relevance(
                                 workspace.target,bond_category);
         double i_unhappiness = workspace.initial.intra_string_unhappiness;
         double t_unhappiness = workspace.target.intra_string_unhappiness;
         // choose string

         //print("about to choose string:");
         //print("initial string: relevance="+i_relevance+", unhappiness="+i_unhappiness);
         //print("target string: relevance="+t_relevance+", unhappiness="+t_unhappiness);
         

         workspace_string string = workspace.initial;
         if ((random.rnd()*(i_relevance+i_unhappiness+t_relevance+t_unhappiness))>
             (i_relevance+i_unhappiness))  string = workspace.target;
         if (string==workspace.initial) print("initial string selected");
         else  print("target string selected");                 

         workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",string.objects);
         // choose neighbour
         print("initial object: "+fromob);
         workspace_object toob = workspace_formulas.choose_neighbor(fromob);
         if (toob==null) {
                          print("object has no neighbour: Fizzle!");
                          return false; }
         print("to object : "+toob);
         slipnode bond_facet = workspace_formulas.choose_bond_facet(fromob,toob);
         if (bond_facet==null) {
                                 print("no possible bond facet: Fizzle");
                                 return false; }
         print("chosen bond facet :"+bond_facet.pname);
         slipnode from_descriptor = workspace_formulas.get_descriptor(fromob,bond_facet);
         slipnode to_descriptor = workspace_formulas.get_descriptor(toob,bond_facet);
         if ((from_descriptor==null)||(to_descriptor==null)){
            print("both objects do not have this descriptor: Fizzle!");
            return false;
         }

         print("from object descriptor: "+from_descriptor.pname);
         print("to object descriptor: "+to_descriptor.pname);
         slipnode b1 = slipnet_formulas.get_bond_category(from_descriptor,to_descriptor);
         slipnode b2 = slipnet_formulas.get_bond_category(to_descriptor,from_descriptor);
         if (b1==slipnet.identity) {
            b1=slipnet.sameness; b2=slipnet.sameness;
         }

         if ((bond_category!=b1)&&(bond_category!=b2)){
            print("no suitable link: Fizzle!");
            return false;

         }
         // there is a possible bond, so propose it
            print(bond_category.pname+" bond proposed");
         if (bond_category==b1)   coderack.propose_bond(fromob,toob,bond_category,bond_facet,from_descriptor, to_descriptor,this);
         else coderack.propose_bond(toob,fromob,bond_category,bond_facet,to_descriptor,from_descriptor,this);
