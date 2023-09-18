$(document).ready(function() {
        $('.scroll-left').click(function() {
          $('.scroll-wrapper').animate({ scrollLeft: '-=200' }, 500);
        });
      
        $('.scroll-right').click(function() {
          $('.scroll-wrapper').animate({ scrollLeft: '+=200' }, 500);
        });
      }); 