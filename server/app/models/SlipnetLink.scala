package models

class SlipnetLink(var from_node: SlipNode, var to_node: SlipNode, var label: SlipNode, var fixed_length: Double) {
  from_node.outgoing_links += this

  var slip_link = false;
  def this(fr: SlipNode, to: SlipNode, lab: SlipNode) = {
    this(fr, to, lab, 0.0)
  }
  def this(fr: SlipNode, to: SlipNode, len: Double) = {
    this(fr, to, null, len)
  }

  def intrinsic_degree_of_association(): Double = {
    if (fixed_length>1.0) {
      100.0-fixed_length
    } else if (label!=null) {
      100.0-label.intrinsic_link_length
    } else 0.0
  }

  def degree_of_association(): Double = {
    if ((fixed_length>0.0)||(label==null)) {
      100.0-fixed_length
    } else {
      label.degree_of_association()
    }
  }

  // GUI
  def draw() = {
    /*Graphics g;
    Color cl;
    int x1,y1,x2,y2;
    int offx1,offy1,offx2,offy2;
    double a;

    g=parentarea.screen;
    if ((Resize)||(ResizeAll)) Calculate_Coors();

    if (!slip_link) g.setColor(GraphicsObject.Grey);
    else g.setColor(new Color(100,100,255));
    g.drawLine(sx1,sy1,sx2,sy2);
    from_node.Draw();
    to_node.Draw();

    Redraw=false;
    Resize = false;*/
  }

}
