import processing.opengl.*;
import codeanticode.glgraphics.*;
import de.fhpotsdam.unfolding.*;
import de.fhpotsdam.unfolding.geo.*;
import de.fhpotsdam.unfolding.utils.*;
import de.bezier.data.sql.*;

de.fhpotsdam.unfolding.Map map;
MySQL msql;
int stackPosition;

List<Location> movTVGeoLocations = new ArrayList<Location>();

public void setup() {
  // setup db
  String user     = "skillup";
  String pass     = "skillup";
  String database = "qori_analyzer";

  // get data
  msql = new MySQL( this, "192.168.4.108", database, user, pass );
  loadGeoData();
    
  // setup graphics
  size(1024, 768, GLConstants.GLGRAPHICS);
  smooth();
  
  map = new de.fhpotsdam.unfolding.Map(this);
  map.zoomToLevel(8);
  map.panTo(new Location(-33.45,-70.6666));
  MapUtils.createDefaultEventDispatcher(this, map);
  
  stackPosition = 0;
}

private void loadGeoData() {
  if ( msql.connect() ) {
    msql.query( "SELECT * FROM reports WHERE uid LIKE '393e8135d97013da9d37f9d0900995f1e473a528' AND latitude > '-71' AND latitude < '-70' AND longitude > '-34' AND longitude < '-33' group by longitude, latitude ORDER BY timestamp ASC LIMIT 10" );
    while ( msql.next() ){
      //println( "id:" + msql.getInt("id") + " uid:" + msql.getString("uid") + " time:" + msql.getLong("timestamp") + " longitude:" + msql.getFloat("longitude") + " latitude:" + msql.getFloat("latitude"));
      movTVGeoLocations.add( new Location(msql.getFloat("longitude"), msql.getFloat("latitude")) );
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

  long sequence = 0;
  for (Location location : movTVGeoLocations) {
    float xy[] = map.getScreenPositionFromLocation(location);
    drawMarker(xy[0], xy[1], sequence);
    sequence += 1;
    if (sequence >= stackPosition){
      break;
    }
  }
  stackPosition += 1;
  if (stackPosition >= movTVGeoLocations.size()) {
    // Finished animation
    //noLoop();
    stackPosition = 0;
  }
}

public void mousePressed() {
  redraw();
  //loop();
}

private void drawMarker(float x, float y, long s) {
  noStroke();
  fill(200, 200, 0, 100);
  ellipse(x, y, 16, 16);
  fill(255, 100);
  ellipse(x, y, 14, 14);
  fill(200, 200, 0, 100);
  ellipse(x, y, 12, 12);
  fill(255, 200);
  ellipse(x, y, 10, 10);
  //fill(0);
  //text(String.valueOf(s), x-8, y-8);
}
