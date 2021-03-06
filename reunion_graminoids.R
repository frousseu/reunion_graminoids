

library(jsonlite)
library(readxl)

#x<-fromJSON("https://api.inaturalist.org/v1/observations/90513306")
#x$results$observation_photos[[1]]$photo$attribution

d<-as.data.frame(read_excel("C:/Users/God/Documents/reunion_graminoids/grasses.xlsx"))
#dcsv<-read.csv("https://raw.githubusercontent.com/frousseu/reunion_graminoids/main/grasses.csv",sep=";")
dcsv<-read.csv("C:/Users/God/Documents/reunion_graminoids/grasses.csv",sep=";",na.strings=c("NA",""))

d$photo<-gsub("/medium.|/small.","/large.",d$photo)

d<-merge(d,dcsv[,c("sp","photo","attribution")],all.x=TRUE) # only get attributions
d<-d[order(d$sp,d$rank),]

d<-unique(d)

#write.table(d[,1:9],"C:/Users/God/Documents/reunion_graminoids/grasses.csv",row.names=FALSE,sep=";",na="")

d$idphoto<-sapply(strsplit(sapply(strsplit(d$photo,"/large."),function(i){if(length(i)==1){NA}else{i[1]}}),"/"),tail,1)
d$idobs<-ifelse(!is.na(d$idphoto),sapply(strsplit(d$obs,"/"),tail,1),NA)
  
### only get attributions for empty ones
#w<-1:nrow(d) # get them all to verify if any attributions have changed
w<-which(!is.na(d$idphoto) & is.na(d$attribution))
for(i in w){ # looping is better cause sometimes it times-out
  if(is.na(d$idobs[i])){
    a<-d$credit[i]
  }else{  
    x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations/",d$idobs[i]))
    m<-match(d$idphoto[i],x$results$observation_photos[[1]]$photo$id)
    a<-x$results$observation_photos[[1]]$photo$attribution[m]
  }
  d$attribution[i]<-a
  Sys.sleep(0.1) # not to make too many requests, but not sure it is relevant
  cat("\r",paste(match(i,w),length(w),sep=" / "))
}

d$attribution[which(is.na(d$attribution))]<-d$credit[which(is.na(d$attribution))]

write.table(d,"C:/Users/God/Documents/reunion_graminoids/grasses.csv",row.names=FALSE,sep=";",na="")


d$flore<-ifelse(is.na(d$flore),"-",d$flore)
d$index<-ifelse(is.na(d$index),"-",d$index)


#d<-d[order(as.integer(is.na(d$photo)),-as.integer(factor(d$family)),d$sp,d$rank),]
d<-d[order(-as.integer(factor(d$family)),d$sp,d$rank),]



css<-function(i){
cat("
<!DOCTYPE html>
  <html>
  <head>
  <link href='https://fonts.googleapis.com/css?family=Roboto Mono' rel='stylesheet'>
  <style>
  body .main-container {
    max-width: 1950px !important;
    width: 1950px !important;
  }
body {
  max-width: 1950px !important;
}
* {
  box-sizing: border-box;
}
p {
  padding: 0px;
  margin: 4px;
  font-family: 'Roboto Mono';
}
a {
  text-decoration: none; /* no underline */
}
.a2 {
  text-decoration: none; /* no underline */
  color: #ccc;
}
.flore {
  color: #FFFFFF77;
  /*font-style: italic;*/
}
.column {
  float: left;
  width: 33%;
  padding: 8px;
  background: red;
}
.species {
  width: 100%;
  padding: 0px;
  background: forestgreen;
}
.species:hover {
  opacity: 0.70;
  filter: alpha(opacity=100);
}
.row::after {
  content: \"\";
  clear: both;
  display: table;
  background: blue;
}
.img2 {
  height:214px;
  width:214px;
  object-fit:cover;
  padding: 2px;
  background: #EEEEEE;
  background-origin: content-box;
  cursor: pointer;
}
.img2:hover {
  opacity: 0.70;
  filter: alpha(opacity=100);
}
.p2 {
  color:#FFFFFFEE;
  font-size:30px;
  font-family:'Helvetica' 
}
#img2 {
  border-radius: 5px;
  cursor: pointer;
  transition: 0.3s;
}

/* The Modal (background) */
.modal {
display: none; /* Hidden by default */
position: fixed; /* Stay in place */
z-index: 1; /* Sit on top */
padding-top: 20px; /* Location of the box */
left: 0;
top: 0;
width: 100%; /* Full width */
height: 100%; /* Full height */
overflow: auto; /* Enable scroll if needed */
background-color: rgb(0,0,0); /* Fallback color */
background-color: rgba(0,0,0,0.9); /* Black w/ opacity */
}

/* Modal Content (image) */
.modal-content {
margin: auto;
display: block;
width: 80%;
max-width: 600px;
}

/* Caption of Modal Image */
#caption {
margin: auto;
display: block;
width: 80%;
max-width: 900px;
text-align: center;
color: #ccc;
padding: 10px 0;
height: 25px;
}

/* Link of Modal Image */
#link {
margin: auto;
display: block;
width: 80%;
max-width: 900px;
text-align: center;
color: #ccc;
padding: 10px 0;
margin-bottom: 50px;
height: 25px;
}

/* Add Animation */
.modal-content, #caption, #link {  
-webkit-animation-name: zoom;
-webkit-animation-duration: 0.3s;
animation-name: zoom;
animation-duration: 0.3s;
}

@-webkit-keyframes zoom {
from {-webkit-transform:scale(0)} 
to {-webkit-transform:scale(1)}
}

@keyframes zoom {
from {transform:scale(0)} 
to {transform:scale(1)}
}

/* The Close Button */
.close {
position: absolute;
top: 15px;
right: 35px;
color: #f1f1f1;
font-size: 40px;
font-weight: bold;
transition: 0.3s;
}

.close:hover,
.close:focus {
color: #bbb;
text-decoration: none;
cursor: pointer;
}

/* 100% Image Width on Smaller Screens */
@media only screen and (max-width: 700px){
.modal-content {
width: 100%;
}
}
</style>
  </head>
  <body>
  <div class=\"species\">
  <h2 style = \"color:#FFFFFFEE;font-size:40px;font-family:'Helvetica';\">Index photographique des poacées, cypéracées et juncacées de la Réunion</h2>
  </div>
  <p style = \"color:black;font-size:17px;\">Cette page est un index photographique des poacées, cypéracées et juncacées de la Réunion. La liste des espèces présentées est basée sur la liste des espèces reconnues comme étant présentes à la Réunion selon <a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore\" target=\"_blank\">l'Index taxonomique de la flore vasculaire de La Réunion</a> du <a href=\"http://www.cbnm.org/\" target=\"_blank\">Conservatoire National Botanique Mascarin (CBN - CPIE Mascarin)</a>. Plusieurs espèces n'ont pas été retenues, car leur mention résultent possiblement d'erreurs d'identification, d'étiquetages ou autres. Voir à la toute fin pour la liste des espèces qui n'ont pas été retenues.</p><br>
  
<p style = \"color:black;font-size:17px;\">La plupart des photos proviennent d'observations déposées sur <a href=\"https://www.inaturalist.org/\" target=\"_blank\">iNaturalist</a> ou de spécimens d'herbiers déposés au <a href=\"https://science.mnhn.fr/institution/mnhn/item/search\" target=\"_blank\">Muséum National d'Histoire Naturelle</a>. La plupart des photos présentées sont toutes sous une license <a href=\"https://creativecommons.org/about/cclicenses/\" target=\"_blank\">Creative Commons (CC)</a> permettant leur utilisation à des fins non-commerciales, mais vérifiez la license et l'auteur de chaque photo en y passant votre curseur ou en cliquant sur la photo et en consultant l'adresse URL au bas. Pour toutes questions ou commentaires: ici</p><br>

<p style = \"color:black;font-size:17px;\">Pour plusieurs espèces, notamment pour quelques espèces endémiques ou rares, seules des photos de spécimens d'herbier sont disponibles. Si vous possédez des photos pour ces espèces et si vous souhaitez contribuer à ce site, merci de déposer vos photos sous forme d'observations sur <a href=\"https://www.inaturalist.org/\" target=\"_blank\">iNaturalist</a> et de me contacter. Finalement, merci de me contacter si vous trouvez des erreurs sur le site ou pour toutes questions ou commentaires. L'identification pour la plupart des photos n'a pas été validée par des experts et je suis moi-même en apprentissage de ces espèces. Il faut donc rester prudent lors de l'utlisation des images présentées ici à des fins d'identification. Pour me contacter: francoisrousseu at hotmail com</p><br>

<div class=\"species\">
<p class=\"p2\" style = \"color:#FFFFFFEE;font-size:30px;\">iNaturalist&nbsp;&nbsp<span class=\"flore\">Flore des Mascareignes&nbsp;&nbsp</span><span class=\"flore\">Index du CBNM</span><span style=\"float:right;\">Famille</span>
</p>
</div>
")

}  
  
species_header<-function(x,i){
  cat(paste0(
 "<a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_taxref=",x$taxref[i]),"\" target=\"_blank\">
   <div class=\"species\">
    <p class=\"p2\">
      ",x$sp[i],"&nbsp;&nbsp<span class=\"flore\">",x$flore[i],"</span>","&nbsp;&nbsp<span class=\"flore\">",x$index[i],"</span>"," <span style=\"float:right;\">",x$family[i],"</span>
    </p>
   </div>  
  </a>
 "))
}

species_photo<-function(x,i){
  cat(paste0(
    "<img class=\"img2\" src=\"",x$photo[i],"\" src2=\"","testing","\" title=\"",paste(x$attribution[i]),"\" alt=\"",paste(x$obs[i]),"\">"
  ))
}

l<-split(d,factor(d$sp,levels=unique(d$sp)))


con <- file("C:/Users/God/Downloads/index.html", open = "wt", encoding = "UTF-8")
sink(con)
css()
invisible(
  lapply(l,function(i){
    species_header(i,1)
    if(!is.na(i$photo[1])){
      invisible(
        sapply(1:nrow(i),function(j){species_photo(i,j)})
      )
    }
  })
)
cat("
<div id=\"myModal\" class=\"modal\">
  <span class=\"close\">&times;</span>
  <img class=\"modal-content\" id=\"img01\">
  <div id=\"caption\"></div>
  <div id=\"link\"></div>
</div>     
")
cat("
<script>

    // create references to the modal...
    var modal = document.getElementById('myModal');
    // to all images -- note I'm using a class!
    var images = document.getElementsByClassName('img2');
    // the image in the modal
    var modalImg = document.getElementById(\"img01\");
    // and the caption in the modal
    var captionText = document.getElementById(\"caption\");
    // and the link in the modal
    var linkText = document.getElementById(\"link\");
    
    modal.addEventListener('click',function(){
    this.style.display=\"none\";
    })
    
    // Go through all of the images with our custom class
    for (var i = 0; i < images.length; i++) {
    var img = images[i];
    // and attach our click listener for this image.
    img.onclick = function(evt) {
    console.log(evt);
    modal.style.display = \"block\";
    modalImg.src = this.src;
    captionText.innerHTML = this.title;
    linkText.innerHTML = '<a class=\"a2\" href=\"' + this.alt + '\" target=\"_blank\">' + this.alt + '</a>' ;
    }
    }
    
    var span = document.getElementsByClassName(\"close\")[0];
    
    span.onclick = function() {
    modal.style.display = \"none\";
    }
    
    </script>    
")
cat("
 </body>
 </html>
")
sink()
close(con)

file.show("C:/Users/God/Downloads/index.html")

