/**
* Created by g.meyer on 23.06.2016.
*
* Labels und sprachabhängige Konfigurationen
*/
if(typeof globalConfig == "undefined") {
globalConfig = {};
}
globalConfig.pickerlanguage = "de";
globalConfig.share = {
"telegram": "https://t.me/share/url?url={{url}}",
"threema": "threema://compose?text={{url}}",
"twitter": "https://www.twitter.com/intent/tweet?text={{title}}:&url={{url}}",
"facebook": "https://www.facebook.com/sharer/sharer.php?u={{url}}",
"googleplus": "https://plus.google.com/share?url={{url}}",
"whatsapp": "whatsapp://send?text={{url}}",
"mail": "mailto:?subject=Empfehlung aus dem Internetauftritt des Bundestages&body=Der nachfolgende Artikel könnte für Sie interessant sein: {{url}}"
};
globalConfig.translations = {
"close" : "schließen",
"noResults" : "Keine Suchergebnisse gefunden",
"errorLoading": "Die Ergebnisse konnte nicht geladen werden.",
"searching": "Suche...",
"endDateBeforeStartDate" : "Ihr Enddatum liegt vor dem Startdatum, bitte wählen Sie ein anderes Datum.",
"dataFormat": "Bitte geben Sie das Datum im Format 'TT.MM.JJJJ' an.",
"yes" : "Ja",
"no" : "Nein",
"neutral" : "Enthalten",
"novote" : "Nicht abgegeben",
"overallresult" : "Gesamtergebnis",
"prev": "Zurück",
"next": "Vor"
};
if(document.querySelector(".bt-formular .form-en")) {
globalConfig.validationMsgs = {
requiredField: 'This is a mandatory field',
badEmail: 'Please insert your E-Mail Adress',
badDate: 'Please use the date format yyyy.mm.dd'
};
globalConfig.dateFormat = "yy.mm.dd";
} else {
globalConfig.validationMsgs = {
requiredField: 'Dieses Feld ist ein Pflichtfeld',
badEmail: 'Bitte geben Sie eine korrekte E-Mail Adresse ein',
badDate: 'Bitte geben Sie ein Datum im Format "tt.mm.jjjj" an'
};
globalConfig.dateFormat = "dd.mm.yy";
}
