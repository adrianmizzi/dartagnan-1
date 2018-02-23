#include "contiki.h"
#include "net/rime/rime.h"

#include <stdio.h>
#include <stdbool.h>
#include "../lib/mycomms.c"
#include "../lib/mysensorutils.c"
#include "../lib/myutils.c"
#include "../lib/myencoderdecoder.c"
#include "../lib/myscheduler.c"

/*---------------------------------------------------------------------------*/
PROCESS(init_process, "Initialise Process");
PROCESS(process_4, "Process 4");
PROCESS(process_6, "Process 6");
PROCESS(process_7, "Process 7");
PROCESS(process_10, "Process 10");
PROCESS(process_14, "Process 14");
AUTOSTART_PROCESSES(&init_process, &scheduler_process);
/*---------------------------------------------------------------------------*/

static process_event_t event_data_ready_4;


static linkaddr_t node1 = { { 21, 59 } };
static linkaddr_t node2 = { { 13, 41 } };
static linkaddr_t node3 = { { 168, 31 } };
static linkaddr_t node4 = { { 207, 37 } };
static linkaddr_t node5 = { { 96, 41 } };

static void recv_uc(struct unicast_conn *c, const linkaddr_t *from)
{
  char *incoming;
  incoming = packetbuf_dataptr();

  printf("Received a message: %s \n", incoming);

  switch (getMessageType(incoming)) {
    case 4 :
      process_start(&process_4, NULL);
      break;
    case 6 :
      process_start(&process_6, NULL);
      break;
    case 7 :
      process_start(&process_7, NULL);
      break;
    case 10 :
      process_start(&process_10, NULL);
      break;
    case 14 :
      process_start(&process_14, NULL);
      break;
    default :
      break;
  }
}

static const struct unicast_callbacks unicast_callbacks = {recv_uc};
/*---------------------------------------------------------------------------*/

PROCESS_THREAD(init_process, ev, data)
{
  PROCESS_BEGIN();

  initialiseTemperatureSensor();

  // allocate the events
  event_data_ready_4 = process_alloc_event();

  init_comms(&unicast_callbacks);

  // schedule jobs in scheduler


  PROCESS_END();
}

PROCESS_THREAD(process_4, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x135,x153 = 0;
  x153 = (int) readTemperature();
  char result[5];
  itoa(x153, result, 10);
  char *message = encode(4, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_6, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x142,x160 = 0;
  x160 = (int) readTemperature();
  char result[5];
  itoa(x160, result, 10);
  char *message = encode(6, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_7, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x141,x159 = 0;
  x159 = (int) readTemperature();
  char result[5];
  itoa(x159, result, 10);
  char *message = encode(7, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_10, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x143,x161 = 0;
  x161 = (int) readTemperature();
  char result[5];
  itoa(x161, result, 10);
  char *message = encode(10, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_14, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x144,x162 = 0;
  x162 = (int) readTemperature();
  char result[5];
  itoa(x162, result, 10);
  char *message = encode(14, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}
