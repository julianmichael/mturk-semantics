package mts

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig

package object tasks {
  val service: RequesterService = new RequesterService(new PropertiesClientConfig("mturk.properties"))
}
