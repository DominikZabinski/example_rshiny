// changing img background via JS, not through shiny observer
$("#jj-type").on("change", function(){
    $("#jj-type").parents(".add_m_b-wrapper").children(".add_m_b-bg").attr("src", "bg"+$(this).val() + ".svg");
    
    if ($(this).val() === "1") {
        $("#jj-type").parents(".add_m_b-wrapper").children(".add_m_b-bg").css("background-image", "linear-gradient( 102.4deg,  rgba(253,189,85,1) 7.8%, rgba(249,131,255,1) 100.3% )");    
    } else {
        $("#jj-type").parents(".add_m_b-wrapper").children(".add_m_b-bg").css("background-image", "radial-gradient( circle farthest-corner at 92.3% 71.5%,  rgba(83,138,214,1) 0%, rgba(134,231,214,1) 90% )");
    }
});

// change via custom message handler (based on genre)