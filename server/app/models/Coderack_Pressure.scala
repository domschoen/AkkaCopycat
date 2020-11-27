package models

import akka.actor.ActorRef
import models.codelet.Codelet

import scala.collection.mutable.ListBuffer


// Should be for GUI
object Coderack_Pressure {
  var Pressures = ListBuffer.empty[Coderack_Pressure] // a list of pressures
  var number_Of_Pressures = 0
  val MaxValue = 0.0001;
  var removed_codelets = ListBuffer.empty[ActorRef]

  def Init_Pressures() = {
    new Coderack_Pressure(0,100,333,250,"BU Bonds","Bottom Up Bonds");
    new Coderack_Pressure(333,100,666,250,"TD Succ Bonds","Top Down Successor Bonds");
    new Coderack_Pressure(666,100,1000,250,"TD Pred Bonds","Top Down Predecessor Bonds");

    new Coderack_Pressure(0,250,333,400,"TD Snms Bonds","Top Down Sameness Bonds");
    new Coderack_Pressure(333,250,666,400,"TD Left Bonds","Top Down Left Bonds");
    new Coderack_Pressure(666,250,1000,400,"TD Right Bonds","Top Down Right Bonds");

    new Coderack_Pressure(0,400,333,550,"TD Succ Grp","Top Down Successor Group");
    new Coderack_Pressure(333,400,666,550,"TD Pred Grp","Top Down Predecessor Group");
    new Coderack_Pressure(666,400,1000,550,"TD Smns Grp","Top Down Sameness Group");

    new Coderack_Pressure(0,550,333,700,"TD Left Grp","Top Down Left Group");
    new Coderack_Pressure(333,550,666,700,"TD Right Grp","Top Down Right Group");
    new Coderack_Pressure(666,550,1000,700,"BU Whole Grp","Bottom Up Whole Group");

    new Coderack_Pressure(0,700,333,850,"Replcmnt Fndr","Replacement Finder");
    new Coderack_Pressure(333,700,666,850,"Rule Cdlts","Rule Codelets");
    new Coderack_Pressure(666,700,1000,850,"Rule Trnsltr","Rule Translator");

    new Coderack_Pressure(0,850,333,1000,"BU Corr","Bottom Up Correspondences");
    new Coderack_Pressure(333,850,666,1000,"IO Corr","Important Object Correspondences");
    new Coderack_Pressure(666,850,1000,1000,"Breakers","Breakers");
  }

/*
  def calculate_Pressures() = {
    var old_max = MaxValue;
    var scale = ((100.0- formulas.temperature)+10.0)/15.0;
    double[] newvals = new double[Number_Of_Pressures];
    double sum = 0.0;
    for (int x=0; x<Number_Of_Pressures; x++){
      double val = 0.0;
      Coderack_Pressure cp = Pressures[x];
      for (int y=0; y<cp.codelets.size(); y++){
        codelet c = (codelet)cp.codelets.elementAt(y);
        val= val +Math.pow((double)(c.urgency),scale);
      }
      newvals[x] = val; sum+=val;
    }
    if (sum==0.0) sum=1.0;
    for (int x=0; x<Number_Of_Pressures; x++){
      Coderack_Pressure cp = Pressures[x];
      double val = newvals[x]/sum;
      if (val>MaxValue) MaxValue = val;
      cp.Values.addElement(new Double(val*100.0));
    }

    for (int x=0; x<removed_codelets.size(); x++){
      codelet c = (codelet)removed_codelets.elementAt(x);
      if (c.Pressure_Type!=null)
        (c.Pressure_Type.codelets).removeElement(c);
    }
    removed_codelets = new Vector();

  }
  def AddCodelet(codelet c) = {
    val tp = if (c.name.equals("bottom-up-bond-scout")) tp = 0;
    if (c.name.equals("top-down-bond-scout--category")){
      slipnode bond_category = (slipnode) c.arguments.elementAt(0);
      if (bond_category==slipnet.successor) tp = 1;
      else if (bond_category==slipnet.predecessor) tp = 2;
      else tp = 3;
    }
    if (c.name.equals("top-down-bond-scout--direction")){
      slipnode bond_category = (slipnode) c.arguments.elementAt(0);
      if (bond_category==slipnet.left) tp = 4;
      else if (bond_category==slipnet.right) tp = 5;
      else tp = 3;
    }
    if (c.name.equals("top-down-group-scout--category")){
      slipnode bond_category = (slipnode) c.arguments.elementAt(0);
      if (bond_category==slipnet.succgrp) tp = 6;
      else if (bond_category==slipnet.predgrp) tp = 7;
      else tp = 8;
    }
    if (c.name.equals("top-down-group-scout--direction")){
      slipnode bond_category = (slipnode) c.arguments.elementAt(0);
      if (bond_category==slipnet.left) tp = 9;
      else if (bond_category==slipnet.right) tp = 10;
    }

    if (c.name.equals("group-scout--whole-string")) tp = 11;
    if (c.name.equals("replacement-finder")) tp = 12;

    if (c.name.equals("rule-scout")) tp = 13;
    if (c.name.equals("rule-translator")) tp = 14;

    if (c.name.equals("bottom-up-correspondence-scout")) tp = 15;
    if (c.name.equals("important-object-correspondence-scout")) tp = 16;
    if (c.name.equals("breaker")) tp = 17;


    if (tp>=0) Pressures[tp].codelets.addElement(c);
    if (c.Pressure_Type!=null) c.Pressure_Type.codelets.addElement(c);
    if (tp>=0) c.Pressure_Type = Pressures[tp];
  }

  public static void RemoveCodelet(codelet c){
    removed_codelets.addElement(c);
  }
*/
}
class Coderack_Pressure(val x1: Int, valy1: Int, val x2: Int, val y2: Int, val text: String, val name: String) {
  // GUI int fontsize, xoff,yoff;
  // GUI Font currfont;
  var clamped = false
  var Filled = false
  var codelets = ListBuffer.empty[ActorRef]
  var unmodified_values = ListBuffer.empty[ActorRef]
  var Tick = 0 // 0 = no tick, 1=tick, 2 = cross;


  // GUI  foreground=Color.black; background=Color.white;
  // GUI  Redraw=false;
  // GUI  Filled = true;
  // GUI  Resize=true;
  Coderack_Pressure.Pressures += this
  Coderack_Pressure.number_Of_Pressures += 1

  // GUI  coderack.CoderackSmall.AddObject(this);





/*
  public static void Reset(){
    MaxValue = 0.0001;
    for (int x=0; x<Number_Of_Pressures; x++){
      Coderack_Pressure cp = Pressures[x];
      cp.unmodified_values=new Vector();
      cp.Values=new Vector();
      cp.codelets = new Vector();
    }
    removed_codelets = new Vector();
  }

  public void CalculateSize(Graphics g){
    Font testfont = new Font("TimesRoman",Font.PLAIN,100);
    g.setFont(testfont);
    int width = g.getFontMetrics().stringWidth(text);
    int height = g.getFontMetrics().getHeight();
    double xratio = ((double)(sx2-sx1))/((double)width);
    double yratio = ((double)(sy2-sy1))/((double)height);
    double fsize;
    if (xratio>yratio) {
      // use yratio to calculate size
      fsize=100.0*yratio;
    }
    else fsize=100.0*xratio;
    if (fsize<1.0) fsize=1.0;
    fsize = fsize * Caption.FontScale;
    currfont = new Font("TimesRoman",Font.PLAIN,(int)fsize);
    g.setFont(currfont);
    xoff = (sx2-sx1-g.getFontMetrics().stringWidth(text))/2;
    yoff = g.getFontMetrics().getDescent()+1;
  }

  public void Draw(){
    Graphics g;
    g=parentarea.screen;
    if (!Redraw) return;
    if (Resize) Calculate_Coors();
    if (Resize) CalculateSize(g);
    if (foreground==null) foreground = Color.black;
    g.setColor(background);
    g.fillRect(sx1,sy1,sx2-sx1,sy2-sy1);
    g.setColor(Color.black);
    g.drawRect(sx1,sy1,sx2-sx1,sy2-sy1);
    if (clamped){
      g.setColor(Color.red);
      g.drawOval(sx1+1,sy1+1,sx2-sx1-2,sy2-sy1-2);
    }
    g.setFont(currfont);
    g.setColor(foreground);
    g.drawString(text,sx1+xoff,sy2-yoff);
    Redraw=false;
    Resize=false;
  }
  public boolean PointerIn(int x, int y){
    if ((x>=sx1)&&(x<=sx2)&&(y>=sy1)&&(y<=sy2)) return true;
    return false;

  }*/
}
