import processing.opengl.*;
import codeanticode.glgraphics.*;
import de.fhpotsdam.unfolding.*;
import de.fhpotsdam.unfolding.geo.*;
import de.fhpotsdam.unfolding.utils.*;
import de.bezier.data.sql.*;

de.fhpotsdam.unfolding.Map map;
MySQL msql;

List<Location> movTVGeoLocations = new ArrayList<Location>();
List<String> movTVGeoDate = new ArrayList<String>();
List<Long> movTVGeoTimestamp = new ArrayList<Long>();

public void setup() {
  // setup graphics
  size(1024, 768, GLConstants.GLGRAPHICS);
  smooth();
  
  // setup db
  String user     = "skillup";
  String pass     = "skillup";
  String database = "qori_analyzer";

  // get data
  msql = new MySQL( this, "192.168.4.108", database, user, pass );
  loadGeoData();
  
  map = new de.fhpotsdam.unfolding.Map(this);
  map.zoomToLevel(12);
  map.panTo(new Location(-33.45,-70.6666));
  MapUtils.createDefaultEventDispatcher(this, map);
}

private void loadGeoData() {
  movTVGeoLocations = new ArrayList<Location>();
  if ( msql.connect() ) {
    msql.query( "SELECT id,uid,event,timestamp,FROM_UNIXTIME(timestamp) AS date,longitude,latitude FROM reports WHERE uid LIKE '5eedd0514bbc4c2c7b77903f13dbf95f4693638f' AND latitude > '-71' AND latitude < '-70' AND longitude > '-34' AND longitude < '-33' group by longitude, latitude ORDER BY timestamp ASC LIMIT 100" );
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

public void draw() {
  background(0);
  map.draw();

  int sequence = 0;
  noFill();
  stroke(#5679C1);
  strokeWeight(1);
  String geoTime;
  Long prevTS=0L;
  
  beginShape();
  for (Location location : movTVGeoLocations) {
    if (sequence > 0){
      prevTS = movTVGeoTimestamp.get(sequence);
    }

    if ( sequence+1 < movTVGeoTimestamp.size() ) {
      if ( movTVGeoTimestamp.get(sequence+1) - prevTS > 1000000 ) {
        endShape();
        beginShape();
      }
    }
    float xy[] = map.getScreenPositionFromLocation(location);
    vertex(xy[0], xy[1]);
    sequence += 1;
  }
  endShape();
  
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
      text(geoTime,xy[0], xy[1]-8);
    }
  }
}

private void drawMarker(float x, float y, long s) {
  noStroke();
  if (s == 0) {
    fill(255, 0, 0, 200);
  }
  else{
    fill(200, 200, 0, 100);
  }
  ellipse(x, y, 16, 16);
  fill(255, 100);
  ellipse(x, y, 14, 14);
  fill(200, 200, 0, 100);
  ellipse(x, y, 12, 12);
  fill(255, 200);
  ellipse(x, y, 10, 10);
}
