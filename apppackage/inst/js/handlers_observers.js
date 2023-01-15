// changing img src via JS, not through shiny observer
$("#jj-type").on("change", function(){
    $("#jj-type").parents(".add_m_b-wrapper").children(".add_m_b-bg").attr("src", "bg"+$(this).val() + ".svg");
});

// changing background-image via custom message handler
Shiny.addCustomMessageHandler("changeBgImgHandler", changeBackgroundImage);

function changeBackgroundImage(message){
    $("#jj-genre").parents(".add_m_b-wrapper").children(".add_m_b-bg").css("background-image", message);
}
