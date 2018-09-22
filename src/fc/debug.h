#ifndef __FILE__
#error __FILE__ does not work!
#endif

#ifndef __LINE__
#error __LINE__ does work!
#endif

#define info(unit,format) if(unit<=debug_level_) write(debug_unit(unit), format)

#define error(X) call debug_error(X,__FILE__,__LINE__)

#define assert(X) if(.not.(X)) error('Faild assertation ('//'X'//')')
