import processing.opengl.*;
import codeanticode.glgraphics.*;
import de.fhpotsdam.unfolding.*;
import de.fhpotsdam.unfolding.geo.*;
import de.fhpotsdam.unfolding.utils.*;
import de.bezier.data.sql.*;
import de.looksgood.ani.*;

de.fhpotsdam.unfolding.Map map;
AniSequence seq;
MySQL msql;
int jump = 0;
float x, y;

List<Location> movTVGeoLocations = new ArrayList<Location>();
List<String> movTVGeoDate = new ArrayList<String>();
List<Long> movTVGeoTimestamp = new ArrayList<Long>();

public void setup() {
  // setup graphics
  size(1024, 768, GLConstants.GLGRAPHICS);
  colorMode(HSB, 100);
  smooth();
  
  // grab middle button
  addMouseWheelListener(new java.awt.event.MouseWheelListener() { 
    public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
      // This gets done before the maps zooms out, therefore it is nearly useless
      // Need to concoct a way to make this run run after the map gets zoomed
      seq.pause();
      //repositionPoint();
      //seq.resume();
      //println("zoooooooooooooooooom!!");
    }
  }); 
  
  // setup animation
  Ani.init(this);
  seq = new AniSequence(this);
  
  // setup point
  x = 0;
  y = 0;
  
  // setup db
  String user     = "skillup";
  String pass     = "skillup";
  String database = "qori_analyzer";

  // get data
  msql = new MySQL( this, "192.168.13.44", database, user, pass );
  loadGeoData();
  
  map = new de.fhpotsdam.unfolding.Map(this);
  map.zoomToLevel(12);
  map.panTo(new Location(-33.45,-70.6666));
  MapUtils.createDefaultEventDispatcher(this, map);
  
  // make animation
  pointWalk();
  
  // start animation
  seq.start();
}

public void draw() {
  background(0);
  map.draw();

  int sequence = 0;
  noFill();
  strokeWeight(1);
  String geoTime;
  Long prevTS=0L;
  int h=0;
  int segs=0;
  int thresh = 60*60*24/4;
  //int thresh = 10000;
  
  beginShape();
  for (Location location : movTVGeoLocations) {
    if (sequence > 0){
      prevTS = movTVGeoTimestamp.get(sequence);
    }

    if ( sequence+1 < movTVGeoTimestamp.size() ) {
      if ( movTVGeoTimestamp.get(sequence+1) - prevTS > thresh ) {
        endShape();
        stroke(h, 100, 100);
        h+=jump;
        segs+=1;
        beginShape();
      }
    }
    
    float xy[] = map.getScreenPositionFromLocation(location);
    vertex(xy[0], xy[1]);
    sequence += 1;
  }
  endShape();
  
  if (jump == 0) {
    jump = (int) 100/segs;
    println("Segments: "+segs);
    println ("Updated jump to: "+jump);
  }
  
  sequence = 0;
  for (Location location : movTVGeoLocations) {
    float xy[] = map.getScreenPositionFromLocation(location);
    drawMarker(xy[0], xy[1], sequence);
    geoTime = (String) movTVGeoDate.get(sequence);
    sequence += 1;
    
    if (dist(mouseX, mouseY, xy[0], xy[1]) < 3) {
      strokeWeight(10);
      point(xy[0], xy[1]);
      fill(0);
      textSize(10);
      textAlign(CENTER);
      text(sequence+": "+geoTime,xy[0], xy[1]-8);
    }
  }
  
  // walking dot
  fill(0, 0, 0, 50);
  ellipse(x,y,10,10);
}

private void loadGeoData() {
  //Alguien en Puerto Montt (882)
  //String uid = "55594a97e86d3f4b4ee6b911b59d98e63f2e23a9";
  //Alguien entre Vina y Santiago (1106)
  //String uid = "4971cb80f69290e1c79a1ed49016ffeab0748e60";
  //Alguien de SUC (2407)
  //String uid = "c4874c2b8910c25d9e5d8e181a6ef3cfe852b7f5";
  //Christian (2215)
  String uid = "5eedd0514bbc4c2c7b77903f13dbf95f4693638f";
  //Nico (2754)
  //String uid = "b0195e18ee9244b78b25d2bf438afd4311041f50";
  //Flight attendant (503)
  //String uid = "6401578e8592675605a79d8e6f79c79dd1383b2c";
  //Chino (417)
  //String uid = "ba7ece3b98db0b8df018ae9e0e84f332bb19a4ee";
  //Luis (4S) (915)
  //String uid = "6f01a0f212db893eca84e8ced20a81b44798031f";
  //Luis (3GS) (3257)
  //String uid = "fa556bbcd8b22d340f92173b121e3ce3e81cafc2";
  //Teppei? (360)
  //String uid = "84b1ccf1d56bf0e70fe2720a623eec7c90b59441";
  //Daniel (337)
  //String uid = "0f25716871b389a9700d3be2f0abd465608281fa";
  //Swedish (301)
  //String uid = "9bce63fa4ec360f9f9ecc93a5632574d2cf6888f";
  //Chilean-Mexican (243)
  //String uid = "716a273bc78882f53e8b1ce8c69653c515e80065";
  //Spanish (197)
  //String uid = "503fd8681433cbef13a9dbf0b8b273ea03c44698";
  //Another Swedish!
  //String uid = "908a0affbd759fc5ad340b663973e52bef26c448";
  //Yet another swedish!
  //String uid = "dbdb64678bee6c8f4502af54ef3c3fa68b24614e";
  //American
  //String uid = "45045A0A-FCF9-5733-86EC-98D4BBC363D4";
  //La Serena/Coquimbo
  //String uid = "9e7456ac8a9b3d0bd6026df1f18965c2e5d0db46";
  //Coquimbo/Calama (503)
  //String uid = "6401578e8592675605a79d8e6f79c79dd1383b2c";
  
  movTVGeoLocations = new ArrayList<Location>();
  if ( msql.connect() ) {
    //msql.query( "SELECT id,uid,event,timestamp,FROM_UNIXTIME(timestamp) AS date,longitude,latitude FROM reports WHERE uid LIKE '"+uid+"' AND latitude > '-71' AND latitude < '-70' AND longitude > '-34' AND longitude < '-33' ORDER BY timestamp ASC LIMIT 100" );
    msql.query( "SELECT id,uid,event,timestamp,FROM_UNIXTIME(timestamp) AS date,longitude,latitude FROM reports WHERE uid LIKE '"+uid+"' ORDER BY timestamp ASC LIMIT 5000" );
    while ( msql.next() ){
      //println( "id:" + msql.getInt("id") + " uid:" + msql.getString("uid") + " time:" + msql.getString("date") + " longitude:" + msql.getFloat("longitude") + " latitude:" + msql.getFloat("latitude"));
      movTVGeoLocations.add( new Location(msql.getFloat("longitude"), msql.getFloat("latitude")) );
      movTVGeoDate.add( msql.getString("date") );
      movTVGeoTimestamp.add( msql.getLong("timestamp") );
    }
  }
  else {
    println( "db connection failed!" );
  }
  println("size: " + movTVGeoLocations.size());
}

private void pointWalk() {
  seq.beginSequence();
  for (Location location : movTVGeoLocations) {
    float xy[] = map.getScreenPositionFromLocation(location);
    //seq.beginStep();
    seq.add(Ani.to(this, 0.05, "x:"+xy[0]+",y:"+xy[1]));
    //seq.endStep();
  }
  seq.endSequence();
}

private void sequenceEnd() {
  //println("sequenceEnd() restart all again");
  seq.start();
}

// pause and resume animation by pressing SPACE
// or press "s" to start the sequence
public void keyPressed() {
  if (key == 's' || key == 'S') seq.start();
  if (key == ' ') {
    if (seq.isPlaying()) seq.pause();
    else seq.resume();
  }
  if (key == 'r' || key == 'R') {
    seq.pause();
    repositionPoint();
    seq.resume();
  }
}

public void mouseDragged(){
  println("drag");
  repositionPoint();
}

public void mousePressed(){
  println("press");
  seq.pause();
}

public void mouseReleased(){
  println("release");
  repositionPoint();
  seq.resume();
}

private void repositionPoint(){
  float pos = seq.getSeek();
  println("Repositioning to "+pos);
  seq = new AniSequence(this);
  pointWalk();
  seq.seek(pos);
}

private void drawMarker(float x, float y, long s) {
  noStroke();
  if (s == 0) {
    fill(0, 0, 0, 50);
    ellipse(x, y, 16, 16);
    fill(0, 100, 100, 50);
    ellipse(x, y, 14, 14);
    fill(0, 0, 0, 50);
    ellipse(x, y, 12, 12);
    fill(0, 100, 100, 50);
    ellipse(x, y, 10, 10);
  }
  else{
    fill(0, 0, 0, 50);
    ellipse(x, y, 16, 16);
    fill(0, 0, 100, 100);
    ellipse(x, y, 14, 14);
    fill(0, 0, 0, 100);
    ellipse(x, y, 12, 12);
    fill(0, 0, 100, 100);
    ellipse(x, y, 10, 10);
  }
  stroke(HSB,100);
}
