if(typeof globalConfig == "undefined") {
globalConfig = {};
}
globalConfig.popupblacklist = [
"/image",
"/blob"
];


globalConfig.cacheHash = true;
globalConfig.map = {
 pathToWkData : "/static/appdata/includes/datasources/wahlkreisergebnisse/btwahl2017/wahlkreise.json",
pathToWahlImages : "/static/appdata/includes/datasources/wahlkreisergebnisse/btwahl2017/wahlImages.json",
pathToBundesImages: "/static/appdata/includes/datasources/wahlkreisergebnisse/btwahl2017/bundesImages.json",
pathToBundeslaender : "/static/appdata/includes/datasources/wahlkreisergebnisse/btwahl2017/bundeslaender.json",
pathToSystemData : "/static/appdata/includes/datasources/wahlkreisergebnisse/btwahl2017/SystemData.json",
pathToInfoMapElements : {
  "alumniWorldmap": "/static/appdata/includes/datasources/infomap/mapelements.json"
}
};
