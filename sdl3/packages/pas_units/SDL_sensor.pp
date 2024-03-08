unit SDL_sensor;

interface

uses
  SDL3_properties;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_sensor_h_}
//{$define SDL_sensor_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_properties.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{*
 *  SDL_sensor.h
 *
 *  In order to use these functions, SDL_Init() must have been called
 *  with the ::SDL_INIT_SENSOR flag.  This causes SDL to scan the system
 *  for sensors, and load appropriate drivers.
  }
type
  PSDL_Sensor = ^TSDL_Sensor;
  TSDL_Sensor = record
      {undefined structure}
    end;

{*
 * This is a unique ID for a sensor for the time it is connected to the system,
 * and is never reused for the lifetime of the application.
 *
 * The ID value starts at 1 and increments from there. The value 0 is an invalid ID.
  }

  PSDL_SensorID = ^TSDL_SensorID;
  TSDL_SensorID = Uint32;
{ The different sensors defined by SDL
 *
 * Additional sensors may be available, using platform dependent semantics.
 *
 * Hare are the additional Android sensors:
 * https://developer.android.com/reference/android/hardware/SensorEvent.html#values
  }
{*< Returned for an invalid sensor  }
{*< Unknown sensor type  }
{*< Accelerometer  }
{*< Gyroscope  }
{*< Accelerometer for left Joy-Con controller and Wii nunchuk  }
{*< Gyroscope for left Joy-Con controller  }
{*< Accelerometer for right Joy-Con controller  }
{*< Gyroscope for right Joy-Con controller  }

  PSDL_SensorType = ^TSDL_SensorType;
  TSDL_SensorType =  Longint;
  Const
    SDL_SENSOR_INVALID = -(1);
    SDL_SENSOR_UNKNOWN = (-(1))+1;
    SDL_SENSOR_ACCEL = (-(1))+2;
    SDL_SENSOR_GYRO = (-(1))+3;
    SDL_SENSOR_ACCEL_L = (-(1))+4;
    SDL_SENSOR_GYRO_L = (-(1))+5;
    SDL_SENSOR_ACCEL_R = (-(1))+6;
    SDL_SENSOR_GYRO_R = (-(1))+7;

{*
 * Accelerometer sensor
 *
 * The accelerometer returns the current acceleration in SI meters per
 * second squared. This measurement includes the force of gravity, so
 * a device at rest will have an value of SDL_STANDARD_GRAVITY away
 * from the center of the earth, which is a positive Y value.
 *
 * values[0]: Acceleration on the x axis
 * values[1]: Acceleration on the y axis
 * values[2]: Acceleration on the z axis
 *
 * For phones and tablets held in natural orientation and game controllers held in front of you, the axes are defined as follows:
 * -X ... +X : left ... right
 * -Y ... +Y : bottom ... top
 * -Z ... +Z : farther ... closer
 *
 * The axis data is not changed when the device is rotated.
 *
 * \sa SDL_GetCurrentDisplayOrientation()
  }
{#define SDL_STANDARD_GRAVITY    9.80665f }
  SDL_STANDARD_GRAVITY = 9.80665;  
{*
 * Gyroscope sensor
 *
 * The gyroscope returns the current rate of rotation in radians per second.
 * The rotation is positive in the counter-clockwise direction. That is,
 * an observer looking from a positive location on one of the axes would
 * see positive rotation on that axis when it appeared to be rotating
 * counter-clockwise.
 *
 * values[0]: Angular speed around the x axis (pitch)
 * values[1]: Angular speed around the y axis (yaw)
 * values[2]: Angular speed around the z axis (roll)
 *
 * For phones and tablets held in natural orientation and game controllers held in front of you, the axes are defined as follows:
 * -X ... +X : left ... right
 * -Y ... +Y : bottom ... top
 * -Z ... +Z : farther ... closer
 *
 * The axis data is not changed when the device is rotated.
 *
 * \sa SDL_GetCurrentDisplayOrientation()
  }
{ Function prototypes  }
{*
 * Get a list of currently connected sensors.
 *
 * \param count a pointer filled in with the number of sensors returned
 * \returns a 0 terminated array of sensor instance IDs which should be freed
 *          with SDL_free(), or NULL on error; call SDL_GetError() for more
 *          details.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetSensors(count:Plongint):PSDL_SensorID;cdecl;external;
{*
 * Get the implementation dependent name of a sensor.
 *
 * \param instance_id the sensor instance ID
 * \returns the sensor name, or NULL if `instance_id` is not valid
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_GetSensorInstanceName(instance_id:TSDL_SensorID):Pchar;cdecl;external;
{*
 * Get the type of a sensor.
 *
 * \param instance_id the sensor instance ID
 * \returns the SDL_SensorType, or `SDL_SENSOR_INVALID` if `instance_id` is
 *          not valid
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorInstanceType(instance_id:TSDL_SensorID):TSDL_SensorType;cdecl;external;
{*
 * Get the platform dependent type of a sensor.
 *
 * \param instance_id the sensor instance ID
 * \returns the sensor platform dependent type, or -1 if `instance_id` is not
 *          valid
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorInstanceNonPortableType(instance_id:TSDL_SensorID):longint;cdecl;external;
{*
 * Open a sensor for use.
 *
 * \param instance_id the sensor instance ID
 * \returns an SDL_Sensor sensor object, or NULL if an error occurred.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_OpenSensor(instance_id:TSDL_SensorID):PSDL_Sensor;cdecl;external;
{*
 * Return the SDL_Sensor associated with an instance ID.
 *
 * \param instance_id the sensor instance ID
 * \returns an SDL_Sensor object.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorFromInstanceID(instance_id:TSDL_SensorID):PSDL_Sensor;cdecl;external;
{*
 * Get the properties associated with a sensor.
 *
 * \param sensor The SDL_Sensor object
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetProperty
 * \sa SDL_SetProperty
  }
function SDL_GetSensorProperties(sensor:PSDL_Sensor):TSDL_PropertiesID;cdecl;external;
{*
 * Get the implementation dependent name of a sensor
 *
 * \param sensor The SDL_Sensor object
 * \returns the sensor name, or NULL if `sensor` is NULL.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_GetSensorName(sensor:PSDL_Sensor):Pchar;cdecl;external;
{*
 * Get the type of a sensor.
 *
 * \param sensor The SDL_Sensor object to inspect
 * \returns the SDL_SensorType type, or `SDL_SENSOR_INVALID` if `sensor` is
 *          NULL.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorType(sensor:PSDL_Sensor):TSDL_SensorType;cdecl;external;
{*
 * Get the platform dependent type of a sensor.
 *
 * \param sensor The SDL_Sensor object to inspect
 * \returns the sensor platform dependent type, or -1 if `sensor` is NULL.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorNonPortableType(sensor:PSDL_Sensor):longint;cdecl;external;
{*
 * Get the instance ID of a sensor.
 *
 * \param sensor The SDL_Sensor object to inspect
 * \returns the sensor instance ID, or 0 if `sensor` is NULL.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorInstanceID(sensor:PSDL_Sensor):TSDL_SensorID;cdecl;external;
{*
 * Get the current state of an opened sensor.
 *
 * The number of values and interpretation of the data is sensor dependent.
 *
 * \param sensor The SDL_Sensor object to query
 * \param data A pointer filled with the current sensor state
 * \param num_values The number of values to write to data
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorData(sensor:PSDL_Sensor; data:Psingle; num_values:longint):longint;cdecl;external;
{*
 * Close a sensor previously opened with SDL_OpenSensor().
 *
 * \param sensor The SDL_Sensor object to close
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_CloseSensor(sensor:PSDL_Sensor);cdecl;external;
{*
 * Update the current state of the open sensors.
 *
 * This is called automatically by the event loop if sensor events are
 * enabled.
 *
 * This needs to be called from the thread that initialized the sensor
 * subsystem.
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_UpdateSensors;cdecl;external;
{ Ends C function definitions when using C++  }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_sensor_h_  }

implementation


end.
